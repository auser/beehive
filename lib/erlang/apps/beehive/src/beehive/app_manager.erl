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
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export ([
  instance/0,
  status/0,
  terminate_all/0,
  terminate_app_instances/1,
  add_application/1, add_application/2,
  spawn_update_bee_status/3,
  request_to_start_new_bee_by_name/1,
  request_to_start_new_bee_by_app/1,
  request_to_update_app/1,
  request_to_expand_app/1,
  request_to_terminate_bee/1,
  garbage_collection/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  queries = queue:new(),
  last_trans = 0
}).

-define (LAUNCHERS_APP_TO_PID, 'launchers_app_to_pid').
-define (LAUNCHERS_PID_TO_APP, 'launchers_pid_to_app').
-define (UPDATERS_PID_TO_APP, 'updaters_pid_to_app').
-define (UPDATERS_APP_TO_PID, 'updaters_app_to_pid').

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
request_to_start_new_bee_by_app(App) -> gen_server:cast(?SERVER, {request_to_start_new_bee_by_app, App}).
request_to_start_new_bee_by_name(Name) -> gen_server:cast(?SERVER, {request_to_start_new_bee_by_name, Name}).
request_to_update_app(App) -> gen_server:cast(?SERVER, {request_to_update_app, App}).
request_to_terminate_bee(Bee) -> gen_server:cast(?SERVER, {request_to_terminate_bee, Bee}).
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
  gen_server:call(?SERVER, {add_application_by_configuration, ConfigProplist, UserEmail}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(Args) -> gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
  % Build the launching ets tables
  Opts = [named_table, set],
  (catch ets:new(?UPDATERS_PID_TO_APP, Opts)),
  (catch ets:new(?UPDATERS_APP_TO_PID, Opts)),
  
  (catch ets:new(?LAUNCHERS_PID_TO_APP, Opts)),
  (catch ets:new(?LAUNCHERS_APP_TO_PID, Opts)),
  
  timer:send_interval(timer:seconds(5), {flush_old_processes}),
  % Try to make sure the pending bees are taken care of by either turning them broken or ready
  timer:send_interval(timer:seconds(5), {manage_pending_bees}),
  % Run maintenance
  % timer:send_interval(timer:seconds(20), {ping_bees}),
  timer:send_interval(timer:minutes(5), {garbage_collection}),
  % timer:send_interval(timer:minutes(2), {maintain_bee_counts}),
  timer:send_interval(timer:minutes(2), {clean_up_apps}),
  
  {ok, #state{}}.

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
handle_call({add_application_by_configuration, ConfigProplist, UserEmail}, From, State) ->
  % NewState = add_application_by_configuration(ConfigProplist, State),
  handle_queued_call(fun() -> add_application_by_configuration(ConfigProplist, UserEmail) end, From, State);

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

handle_cast({request_to_start_new_bee_by_app, App}, State) ->
  start_new_instance_by_app(App),
  {noreply, State};

handle_cast({request_to_start_new_bee_by_name, Name}, State) ->
  case apps:find_by_name(Name) of
    [] -> error;
    App when is_record(App, app) -> start_new_instance_by_app(App)
  end,
  {noreply, State};

handle_cast({request_to_terminate_bee, #bee{status = Status} = Bee}, State) when Status =:= ready ->
  % rpc:cast(Node, app_handler, stop_instance, [Bee]),
  % app_killer_fsm
  {ok, P} = app_killer_fsm:start_link(Bee, self()),
  erlang:link(P),
  app_killer_fsm:kill(P),  
  {noreply, State};

handle_cast({request_to_terminate_bee, _Bee}, State) ->
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
handle_info({bee_terminated, Bee}, State) when is_record(Bee, bee) ->
  ?NOTIFY({bee, bee_terminated, Bee}),
  {noreply, State};

handle_info({clean_up}, State) ->
  {noreply, State};

handle_info({manage_pending_bees}, State) ->
  spawn(fun() ->
    PendingBees = lists:filter(fun(B) -> B#bee.status == pending end, bees:all()),
    lists:map(fun(B) ->
        Status = try_to_connect_to_new_instance(B, 10),
        ?NOTIFY({bee, update_status, B, Status})
      % lists:map(fun(B) ->
        % ?LOG(info, "Garbage cleaning up on: ~p", [Bees#bee.app_name])
      % end, Bees)
    end, PendingBees)
  end),
  {noreply, State};

handle_info({maintain_bee_counts}, State) ->
  spawn(fun() -> maintain_bee_counts() end),
  {noreply, State};

handle_info({flush_old_processes}, State) ->
  spawn(fun() -> flush_old_processes() end),
  {noreply, State};

handle_info({ping_bees}, State) ->
  ping_bees(),
  {noreply, State};

handle_info({garbage_collection}, State) ->
  spawn(fun() -> handle_non_ready_bees() end),
  {noreply, State};
    
handle_info({clean_up_apps}, State) ->
  clean_up(),
  {noreply, State};    

handle_info({'EXIT',_Pid,normal}, State) ->
  {noreply, State};

handle_info({'EXIT',_Pid, {received_unknown_message, {_FsmState, {error, {babysitter, Msg}}}}}, State) ->
  ?LOG(info, "Got received_unknown_message: ~p", [Msg]),
  {noreply, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
  case ets:lookup(?UPDATERS_PID_TO_APP, Pid) of
    [{Pid, App, _Time}] ->
      ets:delete(?UPDATERS_PID_TO_APP, Pid),
      ets:delete(?UPDATERS_APP_TO_PID, App);
    _ -> 
      case ets:lookup(?LAUNCHERS_PID_TO_APP, Pid) of
        [{Pid, App, _Time}] ->
          ets:delete(?LAUNCHERS_PID_TO_APP, Pid),
          ets:delete(?LAUNCHERS_APP_TO_PID, App);
        _ -> true
      end
  end,
  {noreply, State};
  
handle_info({bee_updated_normally, #bee{commit_hash = Sha} = Bee, #app{name = AppName} = App}, State) ->
  % StartedBee#bee{commit_hash = Sha}, App#app{sha = Sha}
  ?LOG(debug, "app_event_handler got bee_started_normally: ~p, ~p", [Bee, App]),
  case apps:find_by_name(AppName) of
    RealApp when is_record(RealApp, app) ->
      apps:save(RealApp#app{sha = Sha, latest_error = undefined});
    _ -> ok
  end,
  case bees:find_by_id(Bee#bee.id) of
    RealBee when is_record(RealBee, bee) -> bees:save(RealBee#bee{lastresp_time = date_util:now_to_seconds()});
    _ -> ok
  end,
  ok = kill_other_bees(Bee),
  {noreply, State};

handle_info({bee_started_normally, _Bee, _App}, State) ->
  {noreply, State};

% {error, {updating, {error, {babysitter, {app,"fake-lvpae",
handle_info({error, {Stage, {error, bee_not_found_after_creation, App}}}, State) ->
  ?LOG(info, "could not find the bee after creation: ~p", [App#app.name]),
  Error = #app_error{
    stage = Stage,
    stderr = "",
    stdout = "Bee could not be found after creating it. Check the repository",
    exit_status = 128,
    timestamp = date_util:now_to_seconds()
  },
  {ok, _NewApp} = apps:save(App#app{latest_error = Error}),
  {noreply, State};
  
handle_info({app_launcher_fsm, error, {_Stage, {error, {babysitter, App}}}}, State) ->
  ?LOG(info, "app_manager caught babysitter error: ~p", [App]),
  {noreply, State};

handle_info({app_launcher_fsm, error, {_Stage, {error, ComandStage, _Ospid, ExitCode, Stderr, Stdout}}, App}, State) ->
  Error = #app_error{
    stage = ComandStage,
    stderr = Stderr,
    stdout = Stdout,
    exit_status = ExitCode,
    timestamp = date_util:now_to_seconds()
  },
  {ok, _NewApp} = apps:save(App#app{latest_error = Error}),
  {noreply, State};

handle_info({error, State, Error}, State) ->
  ?LOG(info, "something died: ~p", [Error]),
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
    RealBee = bees:find_by_id(Bee#bee.id),
    bees:save(RealBee#bee{status = BeeStatus}),
    From ! {updated_bee_status, BeeStatus}
  end).

% Try to connect to the application instance while it's booting up
try_to_connect_to_new_instance(_Bee, 0) -> broken;
try_to_connect_to_new_instance(Bee, Attempts) ->
  ?LOG(info, "try_to_connect_to_new_instance (~p:~p) ~p", [Bee#bee.host, Bee#bee.port, Attempts]),
  case gen_tcp:connect(Bee#bee.host, Bee#bee.port, [binary, {packet, 0}], 500) of
    {ok, Sock} ->
      gen_tcp:close(Sock),
      ready;
    _ -> 
      timer:sleep(500),
      try_to_connect_to_new_instance(Bee, Attempts - 1)
  end.
  
% Add an application based on it's proplist
add_application_by_configuration(ConfigProplist, UserEmail) ->
  case apps:new(ConfigProplist) of
    NewApp when is_record(NewApp, app) ->
      {ok, _App} = apps:save(NewApp),
      {ok, _UserApp} = user_apps:create(UserEmail, NewApp),
      {ok, NewApp};
    E ->
      {error, E}
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
  
% MAINTAIN THE ETS TABLES
flush_old_processes() ->
  lists:map(fun(Tuple) ->
    try_to_clean_up_ets_tables(?UPDATERS_APP_TO_PID, ?UPDATERS_PID_TO_APP, Tuple)
  end, ets:tab2list(?UPDATERS_APP_TO_PID)),
  lists:map(fun(Tuple) ->
    try_to_clean_up_ets_tables(?LAUNCHERS_APP_TO_PID, ?LAUNCHERS_PID_TO_APP, Tuple)
  end, ets:tab2list(?LAUNCHERS_APP_TO_PID)),
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
  % we track the pending instances in app_handler.
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
      bees:update(RealBee#bee{status = NewStatus})
  end.

% Cleanup the bee. Remove traces of the bee from the system
cleanup_bee(#bee{status = terminated} = B) ->
  ?QSTORE:delete_queue(?WAIT_DB, B#bee.app_name);
  % bees:delete(B);
cleanup_bee(B) ->
  (catch app_manager:request_to_terminate_bee(B)),
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

start_new_instance_by_app(App) ->
  case ets:lookup(?LAUNCHERS_APP_TO_PID, App) of
    [Tuple] -> 
      try_to_clean_up_ets_tables(?LAUNCHERS_APP_TO_PID, ?LAUNCHERS_PID_TO_APP, Tuple),
      already_starting_instance;
    _ ->
      app_launcher_fsm_go(?LAUNCHERS_APP_TO_PID, ?LAUNCHERS_PID_TO_APP, launch, App, false)
  end.
  
update_instance_by_app(App) ->
  case ets:lookup(?UPDATERS_APP_TO_PID, App) of
    [Tuple] -> 
      try_to_clean_up_ets_tables(?UPDATERS_APP_TO_PID, ?UPDATERS_PID_TO_APP, Tuple),
      already_updating_instance;
    _ ->
      app_launcher_fsm_go(?UPDATERS_APP_TO_PID, ?UPDATERS_PID_TO_APP, update, App, true)
  end.

expand_instance_by_app(App) -> start_new_instance_by_app(App).

% PRIVATE
app_launcher_fsm_go(AppToPidTable, PidToAppTable, Method, App, Updating) ->
  case App#app.type of
    static -> ok;
    _T ->
      process_flag(trap_exit, true),
      Now = date_util:now_to_seconds(),
      StartOpts = [{app, App}, {caller, self()}, {updating, Updating}],
      case app_launcher_fsm:start_link(StartOpts) of
        {ok, P} ->
          erlang:link(P),
          rpc:call(node(P), app_launcher_fsm, Method, [P]),
          ets:insert(AppToPidTable, {App, P, Now}), 
          ets:insert(PidToAppTable, {P, App, Now}), 
          P;
        Else -> Else
      end
  end.

try_to_clean_up_ets_tables(AppToPidTable, PidToAppTable, {App, Pid, Time}) ->
  case date_util:now_to_seconds() - Time > ?ACTION_TIMEOUT of
    true ->
      true = ets:delete(PidToAppTable, Pid),
      true = ets:delete(AppToPidTable, App);
    false -> ok
  end.

% Kill off all other bees
kill_other_bees(#bee{app_name = Name, id = StartedId, commit_hash = StartedSha} = _StartedBee) ->
  case bees:find_all_by_name(Name) of
    [] -> ok;
    CurrentBees ->
      OtherBees = lists:filter(fun(B) -> B#bee.id =/= StartedId orelse B#bee.commit_hash =/= StartedSha end, CurrentBees),
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
