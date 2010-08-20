%%%-------------------------------------------------------------------
%%% File    : app_manager.erl
%%% Author  : Ari Lerner
%%% Description : The app_manager sits on the router nodes and handles
%%%   outgoing requests dealing with the applications
%%%
%%% Created :  Sun Oct 25 23:54:22 PDT 2009
%%%-------------------------------------------------------------------

-module (app_manager).

-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_cluster).

%% API
-export([start_link/0, stop/0]).
-export ([
  instance/0,
  status/0,
  terminate_all/0,
  terminate_app_instances/1,
  add_application/1, add_application/2,
  update_application/2,
  spawn_update_bee_status/3,
  request_to_start_new_bee_by_name/1, request_to_start_new_bee_by_name/2,
  start_new_bee_by_name/1, start_new_bee_by_name/2,
  request_to_start_new_bee_by_app/1, request_to_start_new_bee_by_app/2,
  start_new_bee_by_app/1, start_new_bee_by_app/2,
  request_to_update_app/1,
  request_to_expand_app/1,
  request_to_terminate_bee/1, request_to_terminate_bee/2,
  terminate_bee/1, terminate_bee/2,
  request_to_save_app/1,
  garbage_collection/0,
  seed_nodes/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
% gen_cluster callback
-export([handle_join/2, handle_leave/3]).

-record(state, {
  run_directory,
  scratch_dir,
  max_bees,               % maximum number of bees on this host
  queries     = queue:new(),
  last_trans  = 0
}).

-define (ACTION_TIMEOUT, 10).
-define (SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%====================================================================
%% Asynchronous methods
%%====================================================================
request_to_expand_app(App) -> gen_server:cast(?SERVER, {request_to_expand_app, App}).
status() -> gen_server:call(?SERVER, {status}).
instance() -> whereis(?SERVER).
garbage_collection() -> gen_server:cast(?SERVER, {garbage_collection}).
% Starting
request_to_start_new_bee_by_app(App) ->  request_to_start_new_bee_by_app(App, undefined).
request_to_start_new_bee_by_app(App, Caller) -> 
  gen_server:cast(?SERVER, {request_to_start_new_bee_by_app, App, Caller}).
request_to_start_new_bee_by_name(Name) -> request_to_start_new_bee_by_name(Name, undefined).
request_to_start_new_bee_by_name(Name, Caller) -> 
  gen_server:cast(?SERVER, {request_to_start_new_bee_by_name, Name, Caller}).

start_new_bee_by_name(Name) -> start_new_bee_by_name(Name, undefined).
start_new_bee_by_name(Name, Caller) ->
  gen_server:call(?SERVER, {start_new_bee_by_name, Name, Caller}, infinity).

start_new_bee_by_app(App) -> start_new_bee_by_app(App, undefined).
start_new_bee_by_app(App, Caller) ->
  gen_server:call(?SERVER, {start_new_bee_by_app, App, Caller}, infinity).

request_to_terminate_bee(Bee) -> request_to_terminate_bee(Bee, undefined).
request_to_terminate_bee(Bee, Caller) -> 
  gen_server:cast(?SERVER, {request_to_terminate_bee, Bee, Caller}).
  
terminate_bee(Bee) -> terminate_bee(Bee, undefined).
terminate_bee(Bee, Caller) -> gen_server:call(?SERVER, {terminate_bee, Bee, Caller}, infinity).
  
request_to_update_app(App) -> gen_server:cast(?SERVER, {request_to_update_app, App}).
request_to_save_app(App) -> gen_server:call(?SERVER, {request_to_save_app, App}).
terminate_app_instances(Appname) -> gen_server:cast(?SERVER, {terminate_app_instances, Appname}).
terminate_all() -> gen_server:cast(?SERVER, {terminate_all}).

%%====================================================================
%% Synchronous methods
%%====================================================================
add_application(ConfigProplist) ->
  case proplists:get_value(user_email, ConfigProplist) of
    undefined -> {error, no_user_email_given};
    V -> add_application(lists:delete(user_email, ConfigProplist), V)
  end.
add_application(ConfigProplist, UserEmail) -> 
  gen_server:call(?SERVER, {add_application, ConfigProplist, UserEmail}).

update_application(Name, ConfigProplist) -> 
  gen_server:call(?SERVER, {update_application, Name, ConfigProplist}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_cluster:start_link({local, ?SERVER}, ?SERVER, [], []).

stop() ->
  gen_cluster:call(?SERVER, {stop}).

%%-------------------------------------------------------------------
%% @spec () ->    Seed::list()
%% @doc List of seed pids
%%      
%% @end
%%-------------------------------------------------------------------
seed_nodes(_State) -> [node(seed_pid())].
seed_pid() -> hd(seed_pids([])).
seed_pids(_State) ->
  case global:whereis_name(?MODULE) of
    undefined -> [self()]; % We are the master
    _ ->
      {ok, Plist} = gen_cluster:plist(?MODULE),
      Plist
  end.


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
init([]) ->
  process_flag(trap_exit, true),
    
  % Try to make sure the pending bees are taken care of by either turning them broken or ready
  timer:send_interval(timer:seconds(5), {manage_pending_bees}),
  % Run maintenance
  % timer:send_interval(timer:seconds(20), {ping_bees}),
  timer:send_interval(timer:minutes(5), {garbage_collection}),
  % timer:send_interval(timer:minutes(2), {maintain_bee_counts}),
  timer:send_interval(timer:minutes(2), {clean_up_apps}),
  
  ScratchDisk = config:search_for_application_value(scratch_dir, ?BEEHIVE_DIR("tmp")),
  RunDir = config:search_for_application_value(squashed_storage, ?BEEHIVE_DIR("run")),
  MaxBackends     = ?MAX_BACKENDS_PER_HOST,
  
  {ok, #state{
    run_directory = RunDir,
    scratch_dir = ScratchDisk,
    max_bees = MaxBackends
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
% Add an application
handle_call({add_application, ConfigProplist, UserEmail}, From, State) ->
  % NewState = add_application(ConfigProplist, State),
  handle_queued_call(fun() -> internal_add_application(ConfigProplist, UserEmail) end, From, State);

handle_call({update_application, Name, ConfigProplist}, From, State) ->
  handle_queued_call(fun() -> internal_update_application(Name, ConfigProplist) end, From, State);

handle_call({request_to_save_app, App}, From, State) ->
  handle_queued_call(fun() -> apps:save(App) end, From, State);

handle_call({start_new_bee_by_name, Name, Caller}, From, State) ->
  case apps:find_by_name(Name) of
    App when is_record(App, app) -> 
      handle_queued_call(fun() -> 
        start_new_instance_by_app(App#app{latest_error=undefined}, Caller)
      end, From, State);
    _ -> {error, app_not_found}
  end;

handle_call({start_new_bee_by_app, App, Caller}, From, State) ->
  handle_queued_call(fun() -> 
    start_new_instance_by_app(App#app{latest_error=undefined}, Caller)
  end, From, State);

handle_call({terminate_bee, Bee, _Caller}, From, State) ->
  handle_queued_call(fun() -> 
    run_app_kill_fsm(Bee, self()),
    receive
      {bee_terminated, NewBee} -> 
        bees:save(NewBee),
        {ok, {bee_terminated, NewBee}};
      X ->
        erlang:display({handle_call, terminate_bee, got, X})
    end
  end, From, State);

% Remove an application from this application server
handle_call({remove_app, AppName}, _From, State) ->
  terminate_app_instances(AppName),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({request_to_update_app, App}, State) ->
  update_instance_by_app(App),
  {noreply, State};

handle_cast({request_to_expand_app, App}, State) ->
  expand_instance_by_app(App),
  {noreply, State};

% STARTING
handle_cast({request_to_start_new_bee_by_app, App, Caller}, State) ->
  start_new_instance_by_app(App, Caller),
  {noreply, State};

handle_cast({request_to_start_new_bee_by_name, Name, Caller}, State) ->
  case apps:find_by_name(Name) of
    App when is_record(App, app) -> start_new_instance_by_app(App, Caller);
    _ -> ok
  end,
  {noreply, State};

handle_cast({request_to_terminate_bee, Bee, Caller}, State) ->
  % app_killer_fsm
  run_app_kill_fsm(Bee, Caller),
  {noreply, State};

handle_cast({garbage_collection}, State) ->
  handle_non_ready_bees(),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------  
handle_info({clean_up}, State) ->
  {noreply, State};

handle_info({manage_pending_bees}, State) ->
  spawn(fun() ->
    PendingBees = lists:filter(fun(B) -> 
        case is_record(B, bee) of
          true -> B#bee.status == pending;
          false -> false 
        end
      end, 
    bees:all()),
    lists:map(fun(B) ->
        Status = try_to_connect_to_new_instance(B, 10),
        ?NOTIFY({bee, update_status, B, Status})
    end, PendingBees)
  end),
  {noreply, State};

handle_info({maintain_bee_counts}, State) ->
  spawn(fun() -> maintain_bee_counts() end),
  {noreply, State};

handle_info({ping_bees}, State) ->
  spawn(fun() -> ping_bees() end),
  {noreply, State};

handle_info({garbage_collection}, State) ->
  spawn(fun() -> handle_non_ready_bees() end),
  {noreply, State};
    
handle_info({clean_up_apps}, State) ->
  spawn(fun() -> clean_up() end),
  {noreply, State};    

handle_info({'EXIT',_Pid,normal}, State) ->
  {noreply, State};

handle_info({'EXIT',_Pid, {received_unknown_message, {_FsmState, {error, {babysitter, Msg}}}}}, State) ->
  ?LOG(debug, "Got received_unknown_message: ~p", [Msg]),
  {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG(debug, "Pid: ~p exited because: ~p", [Pid, Reason]),
  {noreply, State};
  
handle_info({bee_updated_normally, #bee{revision = Sha} = Bee, #app{name = AppName} = App, Caller}, State) ->
  % StartedBee#bee{revision = Sha}, App#app{revision = Sha}
  ?LOG(debug, "app_event_handler got bee_updated_normally: ~p, ~p", [Bee, App]),
  case apps:find_by_name(AppName) of
    RealApp when is_record(RealApp, app) ->
      apps:save(RealApp#app{revision = Sha, latest_error = undefined});
    _ -> ok
  end,
  case bees:find_by_id(Bee#bee.id) of
    RealBee when is_record(RealBee, bee) -> bees:save(RealBee#bee{lastresp_time = date_util:now_to_seconds()});
    _ -> ok
  end,
  Caller ! {bee_updated_normally, Bee, App},
  ok = kill_other_bees(Bee),
  {noreply, State};

handle_info({bee_started_normally, Bee, App, Caller}, State) ->
  ?LOG(debug, "Bee started normally: ~p", [Bee]),
  Caller ! {bee_started_normally, Bee, App},
  {noreply, State};

% {error, {updating, {error, {babysitter, {app,"fake-lvpae",
handle_info({error, {Stage, {error, bee_not_found_after_creation, App}}}, State) ->
  ?LOG(debug, "could not find the bee after creation: ~p", [App#app.name]),
  Error = #app_error{
    stage = Stage,
    stderr = "",
    stdout = "Bee could not be found after creating it. Check the repository",
    exit_status = 128,
    timestamp = date_util:now_to_seconds()
  },
  {ok, _NewApp} = apps:save(App#app{latest_error = Error}),
  {noreply, State};
  
handle_info({app_launcher_fsm, error, {error, broken_start}, Props}, State) ->
  App = proplists:get_value(app, Props),
  Output = proplists:get_value(output, Props),
  
  Error = #app_error{
    stage = launching,
    stdout = Output,
    exit_status = 128, % Erp
    timestamp = date_util:now_to_seconds()
  },
  ?LOG(debug, "app_manager caught error: ~p: ~p", [App, Error]),
  
  {ok, _NewApp} = apps:save(App#app{latest_error = Error}),
  % run_app_kill_fsm(Bee, self()),
  
  case proplists:get_value(caller, Props) of
    undefined -> ok;
    Pid when is_pid(Pid) -> Pid ! {error, broken_start}
  end,
  
  {noreply, State};

handle_info({app_launcher_fsm, error, {StateName, Code}, Props}, State) ->
  App = proplists:get_value(app, Props),
  Output = proplists:get_value(output, Props),
  Bee = proplists:get_value(bee, Props),
  From = proplists:get_value(from, Props),
  
  Error = #app_error{
    stage = StateName,
    stdout = Output,
    exit_status = Code, % Erp
    timestamp = date_util:now_to_seconds()
  },
  ?LOG(debug, "app_manager caught error: ~p", [App, StateName, Code]),
  {ok, _NewApp} = apps:save(App#app{latest_error = Error}),
  
  % Send to the caller
  From ! {error, Error},
  
  run_app_kill_fsm(Bee, self()),
  {noreply, State};

handle_info({error, AState, Error}, State) ->
  erlang:display({handle_info,error,AState,Error}),
  ?LOG(debug, "something died: ~p", [Error]),
  {noreply, State};

handle_info({answer, TransId, Result}, #state{queries = Queries} = State) ->
  case get_transaction(Queries, TransId) of
    {true, From, Q} ->
      gen_server:reply(From, Result);
    {false, Q} ->
      ok
  end,
  {noreply, State#state{queries = Q}};

handle_info(Info, State) ->
  erlang:display({handle_info, Info}),
  ?LOG(info, "app_manager got: ~p", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
  ?LOG(info, "Terminating app_manager because: ~p", [Reason]),
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
handle_join(_JoiningPid, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_leave(LeavingPid, Pidlist, Info, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------
handle_leave(_LeavingPid, _Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_queued_call(Fun, From, #state{queries = OldTransQ, last_trans = LastTrans} = State) ->
  TransId = next_trans(LastTrans),
  FromServer = self(),
  spawn(fun() -> 
    case Fun() of
      {ok, _} = T1 -> FromServer ! {answer, TransId, T1};
      {error, _} = T2 -> FromServer ! {answer, TransId, T2}
    end
  end),
  {noreply, State#state{queries = queue:in({TransId, From}, OldTransQ)}}.

% Spawn a process to try to connect to the instance
spawn_update_bee_status(Bee, From, Nums) ->
  spawn(fun() ->
    BeeStatus = try_to_connect_to_new_instance(Bee, Nums),
    RealBee = case bees:find_by_id(Bee#bee.id) of
      RealBee1 when is_record(RealBee1, bee) -> RealBee1;
      _ -> Bee
    end,
    bees:save(RealBee#bee{status = BeeStatus}),
    From ! {updated_bee_status, BeeStatus}
  end).

% Try to connect to the application instance while it's booting up
try_to_connect_to_new_instance(_Bee, 0) -> broken;
try_to_connect_to_new_instance(Bee, Attempts) ->
  ?LOG(debug, "try_to_connect_to_new_instance (~p:~p) ~p", [Bee#bee.host, Bee#bee.port, Attempts]),
  case gen_tcp:connect(Bee#bee.host, Bee#bee.port, [binary, {packet, 0}], 500) of
    {ok, Sock} ->
      gen_tcp:close(Sock),
      ready;
    _ -> 
      timer:sleep(200),
      try_to_connect_to_new_instance(Bee, Attempts - 1)
  end.
  
% Add an application based on it's proplist
internal_add_application(ConfigProplist, UserEmail) ->
  case apps:create(ConfigProplist) of
    {ok, NewApp} when is_record(NewApp, app) ->
      {ok, _UserApp} = user_apps:create(UserEmail, NewApp),
      {ok, NewApp};
    E ->
      erlang:display({apps,create,failed,E}),
      {error, E}
  end.

internal_update_application(App, ConfigProplist) when is_record(App, app) ->
  case apps:update(App, ConfigProplist) of
    {updated, NewApp} when is_record(NewApp, app) ->
      ValidatedApp = apps:validate_app(NewApp),
      apps:save(ValidatedApp), 
      {ok, ValidatedApp};
    E -> 
      {error, E}
  end;
internal_update_application(Name, ConfigProplist) ->
  case apps:find_by_name(Name) of
    App when is_record(App, app) -> internal_update_application(App, ConfigProplist);
    _ -> {error, app_not_found}
  end.

% Clean up applications
clean_up() ->
  Apps = apps:all(),
  lists:map(fun(App) -> 
    Bees = bees:find_all_by_name(App#app.name),
    RunningBees = lists:filter(fun(B) -> B#bee.status =:= ready end, Bees),
    Proplist = [{num_backends, erlang:length(RunningBees)}],
    clean_up_instances(Bees, App, Proplist) 
  end, Apps).
  
% Clean up the instances
clean_up_instances([], _, Proplist) -> Proplist;
clean_up_instances([Bee|Rest], App, Proplist) -> 
  clean_up_instance(Bee, App, Proplist),
  clean_up_instances(Rest, App, Proplist).

% Cleanup a single instance
clean_up_instance(Bee, App, Proplist) ->
  % If the instance of the application has been used before
  case Bee#bee.lastresp_time of
    0 -> Proplist;
    _Time ->  clean_up_on_app_timeout(Bee, App, Proplist)
  end.
  
% If the instance is not busy, the timeout has been exceeded and there are other application instances running
clean_up_on_app_timeout(#bee{lastresp_time=LastReq} = Bee, #app{timeout=Timeout,sticky=Sticky} = App, Proplist) ->
  _NumBees = proplists:get_value(num_backends, Proplist),
	TimeDiff = date_util:time_difference_from_now(LastReq),
  % ?LOG(info, "clean_up_on_app_timeout: ~p > ~p, ~p > ~p", [NumBees, Min, TimeDiff, Timeout]),
  if
    % stop_instance(Bee, App, From)
    % NumBees > Min andalso 
    TimeDiff > Timeout andalso Sticky =/= true -> 
      ?LOG(debug, "The bee has passed it's prime: ~p > ~p: ~p (sticky: ~p)", [TimeDiff, Timeout, Sticky]),
      ?NOTIFY({bee, terminate_please, Bee});
    true -> clean_up_on_busy_and_stale_status(Bee, App, Proplist)
  end.
    
% If the instance is busy, but hasn't served a request in a long time, kill it
clean_up_on_busy_and_stale_status(#bee{status = Status, lastresp_time = LastReq} = Bee, #app{timeout = Timeout} = App, Proplist) ->
	TimeDiff = date_util:time_difference_from_now(LastReq),
  % ?LOG(info, "clean_up_on_busy_and_stale_status: ~p > ~p + ~p", [TimeDiff, Timeout, ?TIME_BUFFER]),
  if
    Status =:= busy andalso TimeDiff > Timeout + ?TIME_BUFFER -> 
			?NOTIFY({bee, terminate_please, Bee});
    true -> clean_up_on_long_running_instance(Bee, App, Proplist)
  end.

% If the application has been running for a while, kill it
clean_up_on_long_running_instance(#bee{start_time = StartTime} = Bee, _App, Proplist) ->
	TimeDiff = date_util:time_difference_from_now(StartTime),
  % ?LOG(info, "clean_up_on_long_running_instance: ~p > ~p", [TimeDiff, ?RUN_INSTANCE_TIME_PERIOD]),
  if
    TimeDiff > ?RUN_INSTANCE_TIME_PERIOD -> ?NOTIFY({bee, terminate_please, Bee});
    true -> Proplist
  end.

% MAINTENANCE
ping_bees() ->
  ReadyBees = lists:filter(fun(B) -> B#bee.status =:= ready end, bees:all()),
  lists:map(fun(B) ->
    spawn_update_bee_status(B, self(), 10)
  end, ReadyBees),
  ok.
  
% GARBAGE COLLECTION
handle_non_ready_bees() ->
  TerminatedBees = lists:filter(fun(B) -> B#bee.status =:= terminated end, bees:all()),
  [ cleanup_bee(B) || B <- TerminatedBees ],
  
  DownBees = lists:filter(fun(B) -> B#bee.status =/= ready andalso B#bee.sticky =:= false end, bees:all()),
  lists:map(fun(B) ->
    ?LOG(debug, "trying to reconnect to broken bee: ~p", [B]),
    spawn(fun() -> try_to_reconnect_to_bee(B, 5) end)
  end, DownBees),
  ok.

% Maintain bee counts
maintain_bee_counts() ->
  Apps = apps:all(),
  lists:map(fun(App) ->
      AppBees = lists:filter(fun(B) -> B#bee.status =:= ready end, bees:find_all_by_name(App#app.name)),
      NumAppBees = length(AppBees),
      Min = misc_utils:to_integer(App#app.min_instances),
      Max = misc_utils:to_integer(App#app.max_instances),
      case NumAppBees < Min of
        true ->
          % Uh oh, the minimum bees aren't running
          start_number_of_bees(App#app.name, Min - NumAppBees);
        false ->
          case NumAppBees > Max of
            true ->
              % Uh oh, somehow we got too many bees
              ?LOG(debug, "The number of bees running exceeds the number of maximum bees: ~p", [NumAppBees]),
              terminate_number_of_bees(AppBees, NumAppBees - Max);
            false -> ok
          end
      end
    end, Apps),
  ok.

start_number_of_bees(_, 0) -> ok;
start_number_of_bees(Name, Count) ->
  % This entire method will only start 1 instance at a time because
  % But keep this in here for the time being until we should address it
  start_new_instance_by_name(Name),
  start_number_of_bees(Name, Count - 1).
  
terminate_number_of_bees(_, 0) -> ok;
terminate_number_of_bees([Bee|Rest], Count) ->
  ?NOTIFY({bee, terminate_please, Bee}),
  terminate_number_of_bees(Rest, Count - 1).

% Spawned off process to try to "save" the bee
% If not, clean up the instance and delete it from the bees.
% These are throw-aways, so they can come and they can go
try_to_reconnect_to_bee(B, 0) ->
  cleanup_bee(B),
  ok;
try_to_reconnect_to_bee(B, Num) ->
  case try_to_connect_to_new_instance(B, 1) of
    broken -> 
      timer:sleep(200),
      try_to_reconnect_to_bee(B, Num - 1);
    NewStatus ->
      RealBee = bees:find_by_id(B#bee.id),
      bees:update(RealBee#bee.app_name, RealBee#bee{status = NewStatus})
  end.

% Cleanup the bee. Remove traces of the bee from the system
cleanup_bee(#bee{status = terminated} = B) ->
  ?QSTORE:delete_queue(?WAIT_DB, B#bee.app_name);
  % bees:delete(B);
cleanup_bee(B) ->
  (catch app_manager:request_to_terminate_bee(B, self())),
  ?QSTORE:delete_queue(?WAIT_DB, B#bee.app_name).
  % bees:delete(B).

% Starting
% Call spawn to start new instance if the app is not defined as static and
% there is an available host to start the bee on
start_new_instance_by_name(Name) ->
  case apps:find_by_name(Name) of
    [] -> {error, no_app_found};
    App -> start_new_instance_by_app(App)
  end.

start_new_instance_by_app(App) -> start_new_instance_by_app(App, undefined).
start_new_instance_by_app(App, Caller) ->
  app_launcher_fsm_go(launch, App, Caller, false).
  
update_instance_by_app(App) -> update_instance_by_app(App, undefined).
update_instance_by_app(App, Caller) ->
  app_launcher_fsm_go(update, App, Caller, true).

expand_instance_by_app(App) -> start_new_instance_by_app(App).

% PRIVATE
app_launcher_fsm_go(Method, App, Caller, Updating) ->
  case App#app.dynamic of
    static -> ok;
    _T ->
      process_flag(trap_exit, true),
      % Now = date_util:now_to_seconds(),
      StartOpts = [{app, App}, {caller, Caller}, {from, self()}, {updating, Updating}],
      case app_launcher_fsm:start_link(StartOpts) of
        {ok, P} ->
          erlang:link(P),
          rpc:call(node(P), app_launcher_fsm, Method, [P]),
          {ok, P};
        Else -> Else
      end
  end.

% Kill off all other bees
kill_other_bees(#bee{app_name = Name, id = StartedId, revision = StartedSha} = _StartedBee) ->
  case bees:find_all_by_name(Name) of
    [] -> ok;
    CurrentBees ->
      OtherBees = lists:filter(fun(B) -> B#bee.id =/= StartedId orelse B#bee.revision =/= StartedSha end, CurrentBees),
      lists:map(fun(B) -> ?NOTIFY({bee, terminate_please, B}) end, OtherBees),
      ok
  end.

% So that we can get a unique id for each communication
next_trans(I) when I < 268435455 -> I+1;
next_trans(_) -> 1.

get_transaction(Q, I) -> get_transaction(Q, I, Q).
get_transaction(Q, I, OldQ) ->
  case queue:out(Q) of
    {{value, {I, From}}, Q2} ->
      {true, From, Q2};
    {empty, _} ->
      {false, OldQ};
    {_E, Q2} ->
      get_transaction(Q2, I, OldQ)
    end.

run_app_kill_fsm(SentBee, Caller) ->
  case bees:find_by_id(SentBee#bee.id) of
    #bee{pid = Pid} = Bee when is_record(Bee, bee) -> 
      case is_pid(Pid) andalso Bee#bee.os_pid =/= undefined of
        true ->
          {ok, P} = app_killer_fsm:start_link(Bee, Caller),
          % erlang:display({hi, in, request_to_terminate_bee, P}),
          erlang:link(P),
          app_killer_fsm:kill(P),
          P;
        _ -> ok
      end;
    _ -> ok
  end.