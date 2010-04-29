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
  start_link/0, start_link/2,
  stop/0,
  request_to_terminate_bee/1,
  request_to_terminate_all_bees/1,
  add_slave_node/1,
  get_host/0, get_seed/0,
  set_seed/1,
  get_routers/0, get_nodes/0, get_storage/0,
  dump/1,
  join/1,
  notify/1,
  is_a/1,
  can_deploy_new_app/0, can_pull_new_app/0,
  get_next_available_host/0, get_next_available_storage/0,
  find_application_location/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (state, {
  seed,             % seed host
  type,             % type of node (router|bee|storage)
  host              % local host
}).

-define(SERVER, ?MODULE).
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
  Type = config:search_for_application_value(node_type, router, beehive),
  start_link(Type, Seed).

start_link(Type, Seed) ->
  % Init groups
  pg2:create(?ROUTER_SERVERS), pg2:create(?STORAGE_SERVERS), pg2:create(?NODE_SERVERS),
  GroupName = internal_get_group_name(Type),
  SeedArg = case Seed of
    '' -> [];
    E -> E
  end,
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [Type, SeedArg], []) of
    {ok, Pid} = T ->
      pg2:create(GroupName), ok = pg2:join(GroupName, Pid), 
      T;
    Else ->
      ?LOG(error, "Could not start link because ~p~n", [Else]),
      throw({error, {could_not_start_node_manager, Else}})
  end.

%%-------------------------------------------------------------------
%% @spec () ->    ok
%% @doc Stop the node_manager
%%      
%% @end
%%-------------------------------------------------------------------
stop() ->
  gen_server:cast(?SERVER, stop).

% Check how we were started and which node the group is in
is_a(Type) ->
  GroupName = internal_get_group_name(Type),
  lists:member(erlang:whereis(?MODULE), pg2:get_members(GroupName)).

%%-------------------------------------------------------------------
%% @spec (Msg) ->    {ok, Value}
%% @doc Notify the event manager on the routers
%%      
%% @end
%%-------------------------------------------------------------------
notify(Msg) ->
  case is_a(router) of
    true -> ?EVENT_MANAGER:notify(Msg);
    false -> 
      case get_routers() of
        [] -> error;
        Routers -> rpc:call(node(hd(Routers)), ?EVENT_MANAGER, notify, [Msg])
      end
  end.


get_host() -> gen_server:call(?SERVER, {get_host}).

join([]) -> ok;
join(SeedNode) ->
  case net_adm:ping(misc_utils:to_atom(SeedNode)) of
    pong -> ok;
    pang -> error
  end.

get_routers() -> get_node_of_type(router).
get_nodes() -> get_node_of_type(node).
get_storage() -> get_node_of_type(storage).

% Get the members
get_node_of_type(Type) ->
  GroupName = internal_get_group_name(Type),
  pg2:create(GroupName),
  pg2:get_members(GroupName).

% 
request_to_terminate_bee(Bee) ->
  App = apps:find_by_name(Bee#bee.app_name),
  Node = Bee#bee.host_node,
  
  RealBee = bees:find_by_id(Bee#bee.id),
  case RealBee#bee.status of
    % Does the status really matter? This is changed before?
    _ ->
      ok = rpc:call(Node, ?APP_HANDLER, stop_instance, [RealBee, App, self()]),
      bees:save(RealBee#bee{status = terminated})
  end.

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

add_slave_node(SlaveNode) ->
  join(SlaveNode),
  db:add_slave(slave, SlaveNode).

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
init([Type, SeedList]) ->
  process_flag(trap_exit, true),  
  
  Seeds = lists:flatten(convert_to_suitable_seeds([SeedList], [])),
  lists:map(fun(Seed) ->
    case net_kernel:connect_node(Seed) of
      true -> join(Seed);
      false -> ok
    end
  end, Seeds),
  
  db:start(),
    
  timer:send_interval(timer:seconds(10), {stay_connected_to_seed}),
  timer:send_interval(timer:seconds(30), {update_node_stats}),
  % timer:send_interval(timer:minutes(1), {update_node_pings}),
  
  LocalHost = bh_host:myip(),
  
  {ok, #state{
    seed = Seeds,
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
handle_call({set_seed, SeedPid}, _From, #state{type = Type} = State) when is_pid(SeedPid) ->
  ListType = internal_get_group_name(Type),
  pg2:create(ListType),
  ok = pg2:join(ListType, self()),
  join(node(SeedPid)),
  {reply, ok, State#state{seed = SeedPid}};
handle_call({set_seed, SeedNode}, _From, #state{type = Type} = State) ->
  ListType = internal_get_group_name(Type),
  pg2:create(ListType),
  ok = pg2:join(ListType, self()),
  join(SeedNode),
  {reply, ok, State#state{seed = SeedNode}};

handle_call({get_host}, _From, #state{host = Host} = State) ->
  {reply, Host, State};
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
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({request_to_terminate_all_bees, Name}, State) ->
  % First, find all the bees and "unregister" them, or delete them from the bee list so we don't
  % route any requests this way
  Bees = lists:filter(fun(B) -> B#bee.status =:= ready end, bees:find_all_by_name(Name)),
  lists:map(fun(Bee) -> request_to_terminate_bee(Bee) end, Bees),
  % next let's terminate all the bees
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
  
handle_info({stay_connected_to_seed},  #state{seed = SeedNode} = State) ->
  erlang:display(SeedNode),
  case SeedNode of
    [] ->
      {noreply, State};
    Else ->
      {noreply, stay_connected_to_seeds(Else, State)}
  end;
  
handle_info({update_node_stats}, State) ->
  ?NOTIFY({update_node_stats, date_util:now_to_seconds()}),
  {noreply, State};

handle_info({bee_terminated, Bee}, State) ->
  ?NOTIFY({bee, bee_terminated, Bee}),
  {noreply, State};
  
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

% Get group name
internal_get_group_name(Type) ->
  case Type of
    router -> ?ROUTER_SERVERS;
    storage -> ?STORAGE_SERVERS;
    node -> ?NODE_SERVERS;
    _ -> false
  end.

% Convert to suitable seed nodes
convert_to_suitable_seeds([], Acc) -> Acc;
convert_to_suitable_seeds([StrHost|T], Acc) ->
  convert_to_suitable_seeds(T, [misc_utils:to_atom(StrHost)|Acc]).

stay_connected_to_seeds(Seeds,  #state{type = Type} = State) ->
  lists:map(fun(Seed) ->
    case net_kernel:connect_node(Seed) of
      true -> 
        join(Seed),
        {noreply, State};
      false -> 
        RespondingSeed = case ping_node(get_other_nodes(Type)) of
          [] -> Seed;
          E -> 
            case E =:= node(self()) of
              true -> Seed;
              false -> E
            end
        end,
        {noreply, State#state{seed = RespondingSeed}}
    end
  end, Seeds).