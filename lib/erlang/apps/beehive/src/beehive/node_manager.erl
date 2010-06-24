%%% node_manager.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 05/03/10 Ari Lerner <arilerner@mac.com>
%% @doc The basic node_manager to manage the different nodes in beehive
-module (node_manager).
-include ("beehive.hrl").
-include ("common.hrl").

-behaviour(gen_cluster).
-define (DEBUG, false).
-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).


%% API
-export([
  start_count/1,
  start_server/1, start_server/2, start_server/3, start_server/4,
  get_servers/1, get_servers/0,
  start_link/0,
  is_a/1,
  seed_pid/0, seed_pids/1,
  stop/0,
  dump/1,
  get_next_available/1,
  notify/1,
  read_babysitter_config/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_leave/4]).

-record (state, {
  type,             % type of node (router|bee|storage)
  host              % local host
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> 
  start(?SERVER, ?MODULE, [], []).
start_count(Count) -> 
  Pids = lists:map(fun(_X) -> {ok, Pid} = start(?MODULE, ?MODULE, [], []), Pid end, lists:seq(1,Count)),
  {ok, Pids}.

% Start servers
%%-------------------------------------------------------------------
%% @spec (Mod)
%%       (Mod, Args)
%%       (Mod, Args, Opts)
%%       (Name, Mod, Args, Opts)
%%        ->    {ok, Value}
%%              | {error, Reason}
%% @doc Start the node_manager
%%      
%% @end
%%-------------------------------------------------------------------
start_server(Mod) -> start_server(Mod, [], []).
start_server(Mod, Args) -> start_server(Mod, Args, []).
start_server(Mod, Args, Opts) -> start(?MODULE, Mod, Args, Opts).
start_server(Name, Mod, Args, Opts) -> start(Name, Mod, Args, Opts).

start(Name, Mod, Args, Opts) ->
  Seed = config:search_for_application_value(seed, global:whereis_name(node_manager)),
  Type = config:search_for_application_value(node_type, beehive_router),
  
  RealArgs = lists:flatten([[{seed, Seed}, {node_type, Type}], Args]),
  case whereis(Name) of
    undefined -> gen_cluster:start_link({local, Name}, Mod, RealArgs, Opts);
    _ -> gen_cluster:start_link(Mod, RealArgs, Opts)
  end.

%%-------------------------------------------------------------------
%% @spec () ->    List
%% @doc Get all the servers known by the node_manager
%%      
%% @end
%%-------------------------------------------------------------------
get_servers() ->
  {ok, Plist} = gen_cluster:plist(?MODULE),
  Plist.

get_servers(router) -> proplists:get_value(bee_store, get_servers(), []);
get_servers(node) -> proplists:get_value(app_handler, get_servers(), []);
get_servers(storage) -> proplists:get_value(beehive_storage_srv, get_servers(), []);

get_servers(PidType) ->
  case proplists:get_value(PidType, get_servers()) of
    undefined -> [];
    E -> E
  end.

dump(Pid) -> gen_server:call(seed_pid(), {dump, Pid}).

seed_pid() -> hd(seed_pids([])).
seed_pids(_State) -> [global:whereis_name(?MODULE)].

is_a(Type) -> 
  gen_cluster:call(seed_pid(), {is_a, Type}).

notify(Msg) ->
  rpc:call(node(seed_pid()), event_manager, notify, [Msg]),    
  ok.

%%====================================================================
%% server-specific methods
%%====================================================================
%%-------------------------------------------------------------------
%% @spec (Type) ->    Server Node
%% @doc Get the next available server of type::atom()
%%  This will get the next available server that has the lowest
%%  projected load
%%      
%% @end
%%-------------------------------------------------------------------
get_next_available(Type) -> get_next_available(Type, 3).
get_next_available(_Type, 0) -> {error, none};
get_next_available(Type, Count) ->
  case get_servers(Type) of
    [] -> 
      % Should we be starting an instance automagically?
      AtomType = erlang:list_to_atom(lists:flatten(["beehive_", erlang:atom_to_list(Type)])),
      ok = application:start(AtomType),
      timer:sleep(500),
      get_next_available(Type, Count - 1);
    Servers ->
      [{Node, _Weight}|_Rest] = sort_servers_by_load(Servers),
      Node
  end.
  
%%-------------------------------------------------------------------
%% @spec () ->    ok
%% @doc Stop the node_manager
%%      
%% @end
%%-------------------------------------------------------------------
stop() -> gen_cluster:cast(?SERVER, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
  Type = proplists:get_value(node_type, Args, beehive_router),

  timer:send_interval(timer:seconds(30), {update_node_stats}),
    
  % Get babysitter situated
  babysitter_config:init(),  
  read_babysitter_config(),
  
  % Start the database and application
  db:start(),
  application:start(Type),
  timer:send_interval(timer:minutes(1), {update_node_pings}),
  
  LocalHost = bh_host:myip(),
  
  {ok, #state{
    type = Type,
    host = LocalHost
  }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({is_a, QueryType}, _From, #state{type = Type} = State) ->
  Reply = QueryType =:= Type,
  {reply, Reply, State};
handle_call({dump, Pid}, _From, State) ->
  Name = node(Pid),
  Host = rpc:call(Name, bh_host, myip, []),
  Node = #node{ name = Name, host = Host },
  {reply, Node, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, #state{type = Type} = State) ->
  application:stop(Type),
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(JoiningPid, Pidlist, State) ->
  ?TRACE("~p:~p handle join called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_leave(LeavingPid, Pidlist, Info, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------
handle_leave(LeavingPid, Pidlist, Info, State) ->
  ?TRACE("~p:~p handle leave called: ~p, Info: ~p Pidlist: ~p~n", [?MODULE, ?LINE, LeavingPid, Info, Pidlist]),
  {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
sort_servers_by_load(Servers) -> 
  lists:sort(fun({_Node1, Load1},{_Node2, Load2}) -> Load1 < Load2 end, get_server_load(Servers, [])).

get_server_load([], Acc) -> Acc;
get_server_load([H|Rest], Acc) ->
  Stat = rpc:call(node(H), erlang, statistics, [run_queue]),
  get_server_load(Rest, [{H, Stat}|Acc]).

read_babysitter_config() ->
  DefaultConfigDir = filename:join([?BH_ROOT, "etc", "app_templates"]),
  ConfigDir       = misc_utils:to_list(
                      config:search_for_application_value(app_config_dir, DefaultConfigDir)
                    ),
  try
    babysitter_config:read(ConfigDir)
  catch X:Reason ->
    erlang:display({error, {read_babysitter_config, {error, X, Reason}, ConfigDir}}),
    ok
  end.
