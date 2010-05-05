%%%-------------------------------------------------------------------
%%% File    : stoplight_srv.erl
%%% Author  : nmurray@attinteractive.com, alerner@attinteractive.com
%%% Description : Cascading gen_server behavior that implements process clustering.  
%%% See: http://wiki.trapexit.org/index.php/Cascading_Behaviours
%%% Created     : 2009-08-03
%%%
%%% NOTES:
%%% * uses distributed erlang (today)
%%% * registers one global pid in the format of "gen_cluster_" ++
%%%   atom_to_list(Mod) where Mod is the module using gen_cluster. This allows
%%%   for one "rallying point" for each new node that joins the cluster.
%%% * If the node holding the rally point fails, a new node needs to take over the registered name
%%%-------------------------------------------------------------------
-module(gen_cluster).
-include_lib("../include/gen_cluster.hrl").

%% Define this module as a gen_server callback module.
-behaviour(gen_server).

%% Export the same API as gen_server.
-export([start/3, start/4,
    start_link/3, start_link/4,
    call/2, call/3,
    cast/2, reply/2,
    abcast/2, abcast/3,
    multi_call/2, multi_call/3, multi_call/4,
    enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% Export the callbacks that gen_server expects
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Define the behaviour's required callbacks.
-export([behaviour_info/1]).

%% Helper functions
-export([
  leader/1,
  mod_plist/2,
  plist/1,
  add_child/3
]).

behaviour_info(callbacks) ->
    [
    % gen_cluster
      {handle_join, 3}, {handle_leave, 4},
    % gen_server      
      {init,1}, {handle_call,3},{handle_cast,2},{handle_info,2}, {terminate,2},{code_change,3}
   ];

behaviour_info(_) ->
    undefined.

%% State data record.
-record(state, {
  module,       % module name started
  state,        % mod's state
  data,         % user mod's data
  seed,         % seed
  local_plist,  % local proplist of pids
  leader_pids   % pid list of the leader processes
}).

%% debugging helper
-include ("debugger.hrl").

%% Users will use these start functions instead of gen_server's.
%% We add the user's module name to the arguments and call
%% server's start function with our module name instead.
start(Mod, Args, Options) ->
  gen_server:start(?MODULE, [Mod, Args], Options).
start(Name, Mod, Args, Options) ->
  gen_server:start(Name, ?MODULE, [Mod, Args], Options).
start_link(Mod, Args, Options) ->
  gen_server:start(?MODULE, [Mod, Args], Options).
start_link(Name, Mod, Args, Options) ->
  gen_server:start(Name, ?MODULE, [Mod, Args], Options).

%% Delegate the rest of the reqests to gen_server
call(Name, Request) ->
  gen_server:call(Name, Request).
call(Name, Request, Timeout) ->
 gen_server:call(Name, Request, Timeout).
cast(Name, Request) ->
  gen_server:cast(Name, Request).
reply(To, Reply) ->
  gen_server:reply(To, Reply).
abcast(Name, Request) ->
  gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
  gen_server:abcast(Nodes, Name, Request).
multi_call(Name, Req) ->
  gen_server:multi_call(Name, Req).
multi_call(Nodes, Name, Req)  ->
  gen_server:multi_call(Nodes, Name, Req).
multi_call(Nodes, Name, Req, Timeout)  ->
  gen_server:multi_call(Nodes, Name, Req, Timeout).
enter_loop(Mod, Options, State) ->
  gen_server:enter_loop(Mod, Options, State).
enter_loop(Mod, Options, State, Timeout) ->
  gen_server:enter_loop(Mod, Options, State, Timeout).
enter_loop(Mod, Options, State, ServerName, Timeout) ->
  gen_server:enter_loop(Mod, Options, State, ServerName, Timeout).
wake_hib(Parent, Name, State, Mod, Debug) ->
  gen_server:wake_hib(Parent, Name, State, Mod, Debug).

leader(PidRef) ->
  call(PidRef, {'$gen_cluster', leader}).
mod_plist(Type, PidRef) ->
  call(PidRef, {'$gen_cluster', mod_plist, Type}).
plist(PidRef) -> % {ok, Plist}
  call(PidRef, {'$gen_cluster', plist}).
add_child(PidRef, Mod, Pid) ->
  call(PidRef, {'$gen_cluster', join, [{Mod, [Pid]}]}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%
%% run the user's init/1 and store the user's state data in our internal
%% state data record for later reference.
%%--------------------------------------------------------------------

init([Mod, Args]) ->
  Seed = proplists:get_value(seed, Args, undefined),
  LeaderPids = proplists:get_value(leader_pids, Args, []),
  
  ?TRACE("Starting seed", Seed),
  InitialState = #state{module=Mod, local_plist=[{Mod, [self()]}], seed=Seed, leader_pids = LeaderPids},
  {ok, State1} = join_existing_cluster(InitialState),
  {_Resp, State2} = start_cluster_if_needed(State1),
  
  ?TRACE("Starting state", State2),

  case Mod:init(Args) of
    {ok, ExtState} ->
      StateData = State2#state{module = Mod, state = ExtState},
      {ok, StateData};
    {ok, ExtStateName, ExtStateData} -> 
      StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
      {ok, StateData};
    {ok, ExtStateName, ExtStateData, Timeout} ->
      StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
      {ok, StateData, Timeout};
    {stop, Reason} ->
      {stop, Reason};
    Other ->
      ?TRACE("got other:", Other),
      exit(bad_gen_cluster_init_call) % hmmm
  end.

%%--------------------------------------------------------------------
%% Function:  handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                        {reply, Reply, State, Timeout} |
%%                                                      {noreply, State} |
%%                                             {noreply, State, Timeout} |
%%                                          {stop, Reason, Reply, State} |
%%                                                 {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% This join call is called when
handle_call({'$gen_cluster', join, OtherPlist}, From, State) ->
  ?TRACE("$gen_cluster join", State),
  T = handle_node_joining(OtherPlist, From, State),
  {ok, NewState} = T,
  Reply = {ok, NewState#state.local_plist},
  {reply, Reply, NewState};

handle_call({'$gen_cluster', leader}, _From, State) ->
  Reply = global:whereis_name(globally_registered_name(State)),
  {reply, Reply, State};

handle_call({'$gen_cluster', plist}, _From, State) ->
  Reply = {ok, State#state.local_plist},
  {reply, Reply, State};

handle_call({'$gen_cluster', mod_plist, Mod}, _From, State) ->
  Proplist = proplists:get_value(Mod, State#state.local_plist),
  {reply, {ok, Proplist}, State};

handle_call({'$gen_cluster', globally_registered_name}, _From, State) ->
  Reply = {ok, globally_registered_name(State)},
  {reply, Reply, State};

handle_call(Request, From, State) -> 
  Mod = State#state.module,
  ExtState = State#state.state,
  Reply = Mod:handle_call(Request, From, ExtState),
  handle_call_reply(Reply, From, State).

% handle the replies by updating and substituting our own state
handle_call_reply({reply, Reply, ExtState}, _From, State) ->
  NewState = State#state{state=ExtState},
  {reply, Reply, NewState};

handle_call_reply({reply, Reply, ExtState, Timeout}, _From, State) ->
  NewState = State#state{state=ExtState},
  {reply, Reply, NewState, Timeout};

handle_call_reply({noreply, ExtState}, _From, State)  ->
  NewState = State#state{state=ExtState},
  {noreply, NewState};

handle_call_reply({noreply, ExtState, Timeout}, _From, State) ->
  NewState = State#state{state=ExtState},
  {noreply, NewState, Timeout};

handle_call_reply({stop, Reason, Reply, ExtState}, _From, State)  ->
  NewState = State#state{state=ExtState},
  {stop, Reason, Reply, NewState};

handle_call_reply({stop, Reason, ExtState}, _From, State) ->
  NewState = State#state{state=ExtState},
  {stop, Reason, NewState}.
  % handle Other?

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({'$gen_cluster', update_local_state, UpdateFun}, State) ->
  NewState = UpdateFun(State),
  {noreply, NewState};
handle_cast(Msg, State) -> 
  Mod = State#state.module,
  ExtState = State#state.state,
  Reply = Mod:handle_cast(Msg, ExtState),
  handle_cast_info_reply(Reply, State).

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, process, Pid, Info} = T, #state{module = Mod} = State) ->
  ?TRACE("received 'DOWN'. Removing node from list. Info:", Info),
  case handle_node_leaving(Pid, Info, State) of
    {false, #state{state = ExtState} = _NoPidNewState} -> 
      Reply = Mod:handle_info(T, ExtState),
      handle_cast_info_reply(Reply, State);
    {true, TheNewState} when is_record(TheNewState, state) -> 
      {noreply, TheNewState};
    E ->
      erlang:display({?MODULE, ?LINE, error, {unknown, E}})
  end;

handle_info(Info, State) -> 
    ?TRACE("got other INFO", val),
    Mod = State#state.module,
    ExtState = State#state.state,
    Reply = Mod:handle_info(Info, ExtState),
    handle_cast_info_reply(Reply, State).

handle_cast_info_reply({noreply, ExtState}, State) ->
    NewState = State#state{state=ExtState},
    {noreply, NewState};
handle_cast_info_reply({noreply, ExtState, Timeout}, State) ->
    NewState = State#state{state=ExtState},
    {noreply, NewState, Timeout};
handle_cast_info_reply({stop, Reason, ExtState}, State) ->
    NewState = State#state{state=ExtState},
    {stop, Reason, NewState}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    _ = Mod:terminate(Reason, ExtState),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    {ok, NewExtState} = Mod:code_change(OldVsn, ExtState, Extra),
    NewState = State#state{state=NewExtState},
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Func: handle_node_joining(OtherNode, State) -> {ok, NewState}
%% Description: Called when another node joins the server cluster. 
%%--------------------------------------------------------------------
handle_node_joining(OtherPlist, {OtherPid, _Tag}, State) ->  
  ?TRACE("handle_node_joining", OtherPid),
  % Update this across the cluster
  NewStateWithAddedPids = update_all_server_state(State, fun(TheState) ->
    case add_pids_to_plist(OtherPlist, TheState) of
      {ok, NewStateWithAddedPids} -> NewStateWithAddedPids;
      _Else -> TheState
    end
  end),
  
  % callback
  #state{module=Mod, local_plist=Plist, state=ExtState} = NewStateWithAddedPids,
  StateData = case Mod:handle_join(OtherPid, Plist, ExtState) of
    {ok, NewExtState} -> NewStateWithAddedPids#state{state = NewExtState};
    _Else -> NewStateWithAddedPids
  end,
  
  % update the external state
  {ok, StateData}.

handle_node_leaving(Pid, Info, State) ->
  ExtState = State#state.state,
  Mod = State#state.module,
  Bool = does_pid_exist_in_plist(Pid, State),
  
  NewState = update_all_server_state(State, fun(CurrentState) ->
    case does_pid_exist_in_plist(Pid, CurrentState) of
      true ->
        {ok, NewState2} = remove_pid_from_plist(Pid, CurrentState),
        Pidlist = NewState2#state.local_plist,
        {ok, NewExtState} = Mod:handle_leave(Pid, Pidlist, Info, ExtState),
        NewState3 = take_over_globally_registered_name_if_needed(NewState2),
        % NewState3 = NewState2,
        NewState3#state{state=NewExtState};
      false -> State
    end
  end),
  {Bool, NewState}.
  % update_all_server_state


%% Called by global server to update the state of all servers.
%% The result of applying UpdateFun to State is returned, and
%% an update request to all local servers is sent. 
update_all_server_state(State, UpdateFun) ->
  NewState = UpdateFun(State),
  CastFun = fun({_PidMod, Pids}) ->
    lists:map(fun(Pid) ->
      gen_server:cast(Pid, {'$gen_cluster', update_local_state, UpdateFun})
    end, Pids)
  end,
  lists:foreach(CastFun, State#state.local_plist),
  NewState.

%%--------------------------------------------------------------------
%% Func: join_existing_cluster(State) -> {ok, NewState} | false
%% Description: Look for any existing servers in the cluster, try to join them
%%--------------------------------------------------------------------
join_existing_cluster(State) ->
  Servers = get_seed_nodes(State),
  connect_to_servers(Servers),
  global:sync(), % otherwise we may not see the pid yet
  LeaderPids = lists:append([Servers, get_leader_pids(State)]),
  NewState = sync_with_leaders(LeaderPids, State),
  {ok, NewState}.

sync_with_leaders([], State)        -> State;
sync_with_leaders([H|Rest], State)  -> sync_with_leaders(Rest, sync_with_leader(H, State)).

sync_with_leader(Pid, State) when is_pid(Pid) ->
  case is_process_alive(Pid) andalso Pid =/= self() of
    false -> State;
    true ->
      case catch gen_cluster:call(Pid, {'$gen_cluster', join, State#state.local_plist}, 1000) of
        {ok, KnownPlist} ->
          case add_pids_to_plist(KnownPlist, State) of
            {ok, NewInformedState} -> NewInformedState#state{leader_pids = Pid};
            _Else -> State
          end;
        Error ->
          % erlang:display({error, Error}),
          ?TRACE("Error joining", {error, {could_not_join, Error}}),
          State
      end
  end;
sync_with_leader(_, State) -> State.

connect_to_servers(ServerNames) ->
    ?TRACE("servernames", ServerNames),
   ServerRefs = lists:map(fun(Server) ->
      case Server of
      undefined -> 
          ?TRACE("warning, skipping server", Server),
          skip; % do nothing
      _ -> 
         ?TRACE("connecting to server: ", Server),
         connect_to_server(Server)
      end
    end,
    ServerNames),
   {ok, ServerRefs}.

connect_to_server(Pid) when is_pid(Pid) -> connect_to_server(node(Pid));
connect_to_server(Node) ->
  case net_adm:ping(Node) of
    pong -> ok;
    _ ->
      ?TRACE("WARNING: ping of Node failed:", Node) % should this be a bigger failure? how should we handle this so that way the first server doesn't always have to have this problem?
  end.

%%--------------------------------------------------------------------
%% Func: start_cluster_if_needed(State) -> {{ok, yes}, NewState} |
%%                                         {{ok, no}, NewState}
%% Description: Start cluster if we need to
%%--------------------------------------------------------------------
start_cluster_if_needed(State) ->
  global:sync(), % otherwise we may not see the pid yet
  {Resp, NewState} = case whereis_global(State) of
    undefined ->
      start_cluster(State);
    _ ->
      {no, State}
  end,
  {{ok, Resp}, NewState}.

whereis_global(State) -> global:whereis_name(globally_registered_name(State)).

% This needs to be unique to the names
globally_registered_name(#state{module = Mod} = _State) -> Mod.%erlang:list_to_atom("gen_cluster_" ++ atom_to_list(Mod)).

%%--------------------------------------------------------------------
%% Func: start_cluster(State) -> {yes, NewState} | {no, NewState}
%% Description: Start a new cluster, basically just globally register a pid for
%% joining
%%--------------------------------------------------------------------
start_cluster(State) ->
  global:sync(), % otherwise we may not see the other pids yet
  ?TRACE("Starting server:", globally_registered_name(State)),
  RegisterResp = global:re_register_name(globally_registered_name(State), self()),
  {RegisterResp, State}.

% The elements are a list of proplists from the this or other servers  
add_pids_to_plist([{HeadMod, HeadPids}|OtherPids], State) when is_list(HeadPids) ->
  {ok, NewState} = add_pid_list_to_plist(HeadMod, HeadPids, State),
  add_pids_to_plist(OtherPids, NewState);
add_pids_to_plist([{HeadMod, HeadPids}|OtherPids], State) ->
  {ok, NewState} = add_pid_to_plist(HeadMod, HeadPids, State),
  add_pids_to_plist(OtherPids, NewState);
add_pids_to_plist([], State) ->
  {ok, State}.

add_pid_list_to_plist(_OtherMod, [], State) -> {ok, State};
add_pid_list_to_plist(OtherMod, [Head|Tail], State) ->
  {ok, NewState} = add_pid_to_plist(OtherMod, Head, State),
  add_pid_list_to_plist(OtherMod, Tail, NewState).

add_pid_to_plist(OtherMod, OtherPid, #state{local_plist = Plist} = State) ->
  % Exists = lists:any(fun(Elem) -> Elem =:= OtherPid end, State#state.plist),
  NewPlist = case does_pid_exist_in_plist(OtherPid, State) of
    true -> Plist;
    false ->
      erlang:monitor(process, OtherPid),
      % Add it to the proplist
      OtherPlists = proplists:delete(OtherMod, Plist),
      case proplists:get_value(OtherMod, Plist) of
        undefined -> [{OtherMod, [OtherPid]}|OtherPlists];
        OtherPids ->  [{OtherMod, [OtherPid|OtherPids]}|OtherPlists]
      end
  end,
  NewState  = State#state{local_plist = NewPlist},
  {ok, NewState}.

does_pid_exist_in_plist(OtherPid, State) -> % bool()
  case fetch_pid_and_mod_from_proplist(OtherPid, State) of
    {ok, _, OtherPid} -> true;
    {error, not_found, OtherPid} -> false
  end.

% The pid exists, for sure
remove_pid_from_plist(OtherPid, #state{local_plist = Plist} = State) ->
  case fetch_pid_and_mod_from_proplist(OtherPid, State) of
    {error, not_found, OtherPid} -> {ok, State};
    {ok, Mod, OtherPid} -> 
      % Get and remove the current Mod plist
      ListOfModPids = proplists:get_value(Mod, Plist),
      NewListOfModePids = lists:delete(OtherPid, ListOfModPids),

      OtherPlists = proplists:delete(Mod, Plist),
      NewPlist = [{Mod, NewListOfModePids}|OtherPlists],
      NewState  = State#state{local_plist = NewPlist},
      {ok, NewState}
  end.

% Fetch pid from the proplist
fetch_pid_and_mod_from_proplist(Pid, #state{local_plist = Plist} = _State) ->
  Keys = proplists:get_keys(Plist),
  F = fun(List) -> lists:any(fun(Elem) -> Elem =:= Pid end, List) end,
  G = fun(Key) -> proplists:get_value(Key, Plist) end,

  {ContainingList,_} = lists:partition(fun(Key) -> F(G(Key)) end, Keys),
  case ContainingList of
    [] -> {error, not_found, Pid};
    [Node] -> {ok, Node, Pid}
  end.
    
take_over_globally_registered_name_if_needed(State) -> % NewState
  case need_to_take_over_globally_registered_name(State) of
    true -> 
      {_YesNo, NewState} = start_cluster(State),  
      NewState;
    false -> State
  end. 

need_to_take_over_globally_registered_name(State) -> % bool()
  case whereis_global(State) of
    undefined -> true;
    Pid ->  
    case is_process_alive(Pid) of
      true -> false;
      false -> true
    end
  end.

% list of Nodes
% Node will be sent to net_adm:ping
get_seed_nodes(State) ->
  Servers = [],
  Mod = State#state.module,
  ExtState = State#state.state,
  case erlang:module_loaded(Mod) of
    true -> ok;
    false -> code:load_file(Mod)
  end,
  Servers1 = case erlang:function_exported(Mod, seed_nodes, 1) of
    true -> [Mod:seed_nodes(ExtState)|Servers];
    false -> Servers
  end,
  Servers2 = case init:get_argument(gen_cluster_known) of
    {ok, [[Server]]} ->
      lists:append([list_to_atom(Server), Servers1]);
     _ ->
      case State#state.seed of
        [Server|_Servers] ->
           % [{Server, undefined}]; % ??
           [Server|Servers1]; % ??
        _ ->
         Servers1 
      end
  end,
  % file should be of the format
  %
  % "node@nohost".
  % "foo@bar".
  Servers3 = case os:getenv("GEN_CLUSTER_SEED_CONFIG") of
    false -> Servers2;
    File ->
      case file:consult(File) of
        {ok, Terms} -> lists:append(Servers2, Terms);
        _ -> Servers2
      end
  end,  
  Servers3.

get_leader_pids(#state{module = Mod, leader_pids = LeaderPid, seed = Seed, state = ExtState} = State) ->
  SeedPids = case Seed of
    undefined -> [];
    X1 -> [X1]
  end,
  Pids = case LeaderPid of
    undefined -> SeedPids;
    PidList when is_list(PidList) -> lists:flatten([PidList, SeedPids]);
    E when is_pid(E) -> [E|SeedPids]
  end,
  Pids2 = case erlang:function_exported(Mod, leader_pids, 1) of
    true -> 
      case Mod:leader_pids(ExtState) of
        undefined -> Pids;
        [undefined] -> Pids;
        List when is_list(List) -> lists:append([List, Pids]);
        APid when is_pid(APid) -> [APid|Pids]
      end;
    false -> Pids
  end,
  Pids3 = case whereis_global(State) of % join unless we are the main server 
    undefined -> Pids2;
    X when X =:= self() -> Pids2;
    Pid -> [Pid|Pids2]
  end,
  lists:usort(Pids3). % get unique elements