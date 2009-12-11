%%%-------------------------------------------------------------------
%%% File    : node_manager.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Nov 17 10:29:53 PST 2009
%%%-------------------------------------------------------------------

-module (node_manager).
-include ("common.hrl").
-include ("beehive.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  start_link/2,
  request_to_terminate_bee/1,
  request_to_terminate_all_bees/1,
  get_host/0, get_seed/0,
  set_seed/1,
  get_routers/0, get_nodes/0, get_storage/0,
  dump/1,
  join/1,
  can_deploy_new_app/0, can_pull_new_app/0,
  get_next_available_host/0, get_next_available_storage/0,
  find_application_location/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (state, {
  seed,             % seed host
  type,             % type of node (router|node)
  host              % local host
}).

-define(SERVER, ?MODULE).
-define (ROUTER_SERVERS, 'ROUTER SERVERS').
-define (NODE_SERVERS, 'NODE SERVERS').
-define (STORAGE_SERVERS, 'STORAGE SERVERS').
% Other modules
-define (APP_HANDLER, app_handler).
-define (STORAGE_SRV, bh_storage_srv).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> 
  Seed = config:search_for_application_value(seed, node(), beehive),
  case config:search_for_application_value(node_type, router, beehive) of
    router -> start_link(router, Seed);
    storage -> start_link(storage, Seed);
    node -> start_link(node, Seed)
  end.
  
start_link(router, Seed) -> 
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [router, Seed], []) of
    {ok, Pid} ->
      pg2:create(?ROUTER_SERVERS),
      ok = pg2:join(?ROUTER_SERVERS, Pid),
      {ok, Pid};
    Else ->
      ?LOG(error, "Could not start router link: ~p~n", [Else])
  end;

start_link(storage, Seed) ->
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [storage, Seed], []) of
    {ok, Pid} ->
      pg2:create(?STORAGE_SERVERS),
      ok = pg2:join(?STORAGE_SERVERS, Pid),
      {ok, Pid};
    Else ->
      ?LOG(error, "Could not start router link: ~p~n", [Else])
  end;  

start_link(node, Seed) -> 
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [node, Seed], []) of
    {ok, Pid} ->
      pg2:create(?NODE_SERVERS),
      ok = pg2:join(?NODE_SERVERS, Pid),
      {ok, Pid};
    Else ->
      ?LOG(error, "Could not start router link: ~p~n", [Else])
  end.
  
get_host() -> gen_server:call(?SERVER, {get_host}).

join([]) -> ok;
join(SeedNode) ->
  case net_adm:ping(SeedNode) of
    pong -> ok;
    pang -> error
  end.

get_routers() -> 
  pg2:create(?ROUTER_SERVERS),
  pg2:get_members(?ROUTER_SERVERS).
  
get_nodes() -> 
  pg2:create(?NODE_SERVERS),
  pg2:get_members(?NODE_SERVERS).

get_storage() ->
  pg2:create(?STORAGE_SERVERS),
  pg2:get_members(?STORAGE_SERVERS).  

request_to_terminate_bee(Bee) ->
  gen_server:cast(?SERVER, {request_to_terminate_bee, Bee}).

request_to_terminate_all_bees(Name) ->
  gen_server:cast(?SERVER, {request_to_terminate_all_bees, Name}).

dump(Pid) ->
  gen_server:call(?SERVER, {dump, Pid}).
  
get_seed() ->
  gen_server:call(?SERVER, {get_seed}).
  
set_seed(SeedPid) ->
  gen_server:call(?SERVER, {set_seed, SeedPid}).

can_deploy_new_app() ->
  gen_server:call(?SERVER, {can_deploy_new_app}).
    
can_pull_new_app() ->
  gen_server:call(?SERVER, {can_pull_new_app}).
  
get_next_available_host() ->
  get_next_available(?NODE_SERVERS, length(get_nodes())+1, ?APP_HANDLER, can_deploy_new_app, []).

get_next_available_storage() ->
  get_next_available(?STORAGE_SERVERS, length(get_storage())+1, ?STORAGE_SRV, can_pull_new_app, []).

find_application_location(AppName) ->
  get_next_available(?STORAGE_SRV, length(get_storage())+1, ?STORAGE_SRV, find_app, [AppName]).  

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
init([Type, Seed]) ->
  process_flag(trap_exit, true),  
  join(Seed),
  
  SlaveDb = case Type of
    router ->
      case Seed of
        [] -> 
          % Initializing root router
          case db:already_initialized() of
            true -> false;
            false -> db:init(), false
          end;
        _ -> true
      end;
    storage -> true;
    node ->
      % Node initialization stuff
      true
  end,
  
  case SlaveDb of
    true ->
      ?LOG(info, "Initializing slave db: ~p", [Seed]),
      mesh_util:init_db_slave(Seed);
    false -> ok
  end,
  
  timer:send_interval(timer:seconds(10), {stay_connected_to_seed}),
  % timer:send_interval(timer:minutes(1), {update_node_pings}),
  
  LocalHost = host:myip(),
  
  {ok, #state{
    seed = Seed,
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
% Global can deploy new app
handle_call({can_deploy_new_app}, _From, State) ->
  Nodes = lists:map(fun(N) -> node(N) end, get_nodes()),
  {Responses, _} = rpc:multicall(Nodes, ?APP_HANDLER, can_deploy_new_app, [], timer:seconds(3)),
  Reply = lists:member(true, Responses),
  {reply, Reply, State};

% Global can pull new app
handle_call({can_pull_new_app}, _From, State) ->
  Nodes = lists:map(fun(N) -> node(N) end, get_storage()),
  {Responses, _} = rpc:multicall(Nodes, ?STORAGE_SRV, can_pull_new_app, [], timer:seconds(3)),
  Reply = lists:member(true, Responses),
  {reply, Reply, State};

handle_call({get_seed}, _From, #state{seed = Seed} = State) ->
  {reply, Seed, State};

handle_call({set_seed, SeedPid}, _From, #state{type = Type} = State) ->
  ListType = case Type of
    router -> ?ROUTER_SERVERS;
    storage -> ?STORAGE_SERVERS;
    node -> ?NODE_SERVERS
  end,
  pg2:create(ListType),
  ok = pg2:join(ListType, self()),
  join(SeedPid),
  {reply, ok, State#state{seed = SeedPid}};
handle_call({get_host}, _From, #state{host = Host} = State) ->
  {reply, Host, State};
handle_call({dump, Pid}, _From, State) ->
  Name = node(Pid),
  Host = rpc:call(Name, host, myip, []),
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
handle_cast({request_to_terminate_all_bees, Name}, State) ->
  % First, find all the bees and "unregister" them, or delete them from the bee list so we don't
  % route any requests this way
  Backends = bees:find_all_by_name(Name),
  lists:map(fun(Backend) -> bees:update(Backend#bee{status = unavailable}) end, Backends),
  % Next, do this in an rpc call to shutdown the nodes
  App = apps:find_by_name(Name),
  Nodes = lists:map(fun(N) -> node(N) end, get_nodes()),
  % Can't do this in a multicall (or it would be an optimization), but want to get the node back
  % so for now, we'll do it in an rpc:call, instead
  lists:map(fun(Node) ->
    case rpc:call(Node, ?APP_HANDLER, has_app_named, [Name]) of
      true -> 
        rpc:call(Node, ?APP_HANDLER, stop_app, [App, self()]);
      false -> ok
    end
  end, Nodes),
  {noreply, State};

handle_cast({request_to_terminate_bee, Bee}, State) ->
  App = apps:find_by_name(Bee#bee.app_name),
  Nodes = lists:map(fun(N) -> node(N) end, get_nodes()),
  lists:map(fun(Node) ->
    case rpc:call(Node, ?APP_HANDLER, has_app_named, [Bee#bee.app_name]) of
      true -> 
        rpc:call(Node, ?APP_HANDLER, stop_instance, [Bee, App, self()]);
      false -> ok
    end
  end, Nodes),
  {noreply, State};
  
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({stopped_bee, Bee}, State) ->
  RealBee = bees:find_by_id(Bee#bee.id),
  bees:update(RealBee#bee{status = down}),
  {noreply, State};
  
handle_info({stay_connected_to_seed}, #state{seed = SeedNode, type = Type} = State) ->
  case SeedNode of
    [] ->
      {noreply, State};
    _Else ->
      case net_adm:ping(SeedNode) of
        pong -> 
          {noreply, State};
        pang -> 
          RespondingSeed = case ping_node(get_other_nodes(Type)) of
            [] -> SeedNode;
            E -> 
              case E =:= node(self()) of
                true -> SeedNode;
                false -> E
              end
          end,
          {noreply, State#state{seed = RespondingSeed}}
      end
  end;
  
handle_info(Info, State) ->
  ?LOG(info, "handle_info: ~p ~p", [?MODULE, Info]),
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
%%% Internal functions
%%--------------------------------------------------------------------
% Appropriate here... I think
% internal
get_next_available(_, 0, _, _, _) -> false;
get_next_available(Group, Count, M, F, A) ->
  case (catch mesh_util:get_random_pid(Group)) of
    {ok, Pid} ->
      Node = node(Pid),
      case rpc:call(Node, M, F, A, 1000) of
        {badrpc, _} -> get_next_available(Group, Count - 1, M, F, A);
        true -> Node;
        false -> get_next_available(Group, Count - 1, M, F, A)
      end;
    {error, _Reason} -> false
  end.
  
% Get the next nodes of the same type
get_other_nodes(Type) ->
  case Type of
    node -> get_nodes();
    storage -> get_storage();
    router -> get_routers()
  end.

% Go through a list of nodes, ping them and return the first one that responds
ping_node([]) -> [];
ping_node([H|Rest]) ->
  case net_adm:ping(node(H)) of
    pong -> node(H);
    pang -> ping_node(Rest)
  end.
