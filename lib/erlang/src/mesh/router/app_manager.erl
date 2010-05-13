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
  add_application/1,
  spawn_update_bee_status/3,
  request_to_start_new_bee_by_name/1,
  request_to_start_new_bee_by_app/1,
  request_to_update_app/1,
  request_to_terminate_bee/1,
  garbage_collection/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  app_dir,              % Default directory of applications
  active_apps = [],     % Active applications (that have their socket handled via proxy_handler)
  dead_apps = []        % Apps that have been killed and or are dead
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
status() -> gen_server:call(?SERVER, {status}).
instance() -> whereis(?SERVER).

terminate_all() -> gen_server:cast(?SERVER, {terminate_all}).
terminate_app_instances(Appname) -> gen_server:cast(?SERVER, {terminate_app_instances, Appname}).
  
add_application(ConfigProplist) -> gen_server:call(?SERVER, {add_application_by_configuration, ConfigProplist}).

request_to_update_app(App) -> gen_server:cast(?SERVER, {request_to_update_app, App}).
request_to_start_new_bee_by_app(App) -> gen_server:cast(?SERVER, {request_to_start_new_bee_by_app, App}).
request_to_start_new_bee_by_name(Name) -> gen_server:cast(?SERVER, {request_to_start_new_bee_by_name, Name}).
request_to_terminate_bee(Bee) -> gen_server:cast(?SERVER, {request_to_terminate_bee, Bee}).
  
garbage_collection() -> gen_server:cast(?SERVER, {garbage_collection}).

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
handle_call({add_application_by_configuration, ConfigProplist}, _From, State) ->
  NewState = add_application_by_configuration(ConfigProplist, State),
  {reply, ok, NewState};

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
% Terminate all instances of every app
% handle_cast({terminate_all}, State) ->
%   ?LOG(info, "Terminating all apps", []),
%   lists:map(fun(_Name, Bees) ->
%     lists:map(fun(Bee) -> app_handler:stop_instance(Bee, State) end, Bees)
%     end, router_srv:all(instances)),
%   {reply, ok, State};
  
% Terminate all the instances of a certain application
% handle_cast({terminate_app_instances, AppName}, State) ->
%   Bees = router_srv:lookup(instances, AppName),
%   lists:map(fun(Bee) -> app_handler:stop_instance(Bee, State) end, Bees),
%   router_srv:store(instances, AppName, []),
%   {noreply, State};
% request_to_update_app(App) -> gen_server:cast(?SERVER, {request_to_update_app, App}).
% request_to_start_new_bee_by_app(App) -> gen_server:cast(?SERVER, {request_to_start_new_bee_by_app, App}).

handle_cast({request_to_update_app, App}, State) ->
  erlang:display({request_to_update_app, ets:lookup(?UPDATERS_APP_TO_PID, App)}),
  case ets:lookup(?UPDATERS_APP_TO_PID, App) of
    [{_App, _Pid, Time}] -> 
      ?LOG(info, "Cannot launch app as there is already one in progress (timeout: ~p)", [date_util:now_to_seconds() - Time]),
      already_updating_app;
    _ ->
      Now = date_util:now_to_seconds(),
      Pid = node_manager:get_next_available(storage),
      Node = node(Pid),
      ets:insert(?UPDATERS_APP_TO_PID, {App, Pid, Now}),
      ets:insert(?UPDATERS_PID_TO_APP, {Pid, App, Now}),
      Self = self(),
      case rpc:call(Node, bh_storage_srv, fetch_or_build_bee, [App, Self]) of
        {bee_built, _Proplists} -> 
          % NewApp = apps:update_proplist_for_app(App, Proplists),
          case start_new_instance_by_app(App) of
            true -> 
              ets:delete(?UPDATERS_PID_TO_APP, Pid),
              ets:delete(?UPDATERS_APP_TO_PID, App),
              ok;
            Else ->
              erlang:display({start_new_instance_by_app, in, request_to_start_new_bee_by_app, Else})
          end;
        Else ->
          Else
      end
  end,
  {noreply, State};
  
handle_cast({request_to_start_new_bee_by_app, App}, State) ->
  start_new_instance_by_app(App),
  {noreply, State};
  
handle_cast({request_to_start_new_bee_by_name, Name}, State) ->
  case apps:find_by_name(Name) of
    [] -> error;
    App -> start_new_instance_by_app(App)
  end,
  {noreply, State};

handle_cast({request_to_terminate_bee, Bee}, State) ->
  app_handler:stop_instance(Bee),
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
handle_info({bee_terminated, Bee}, State) ->
  ?NOTIFY({bee, bee_terminated, Bee}),
  {noreply, State};

handle_info({clean_up}, State) ->
  {noreply, State};

handle_info({manage_pending_bees}, State) ->
  PendingBees = lists:filter(fun(B) -> B#bee.status == pending end, bees:all()),
  lists:map(fun(B) ->
      Status = try_to_connect_to_new_instance(B, 10),
      ?NOTIFY({bee, update_status, B, Status})
    % lists:map(fun(B) ->
      % ?LOG(info, "Garbage cleaning up on: ~p", [Bees#bee.app_name])
    % end, Bees)
  end, PendingBees),
  {noreply, State};

handle_info({maintain_bee_counts}, State) ->
  maintain_bee_counts(),
  {noreply, State};

handle_info({flush_old_processes}, State) ->
  flush_old_processes(),
  {noreply, State};

handle_info({ping_bees}, State) ->
  ping_bees(),
  {noreply, State};

handle_info({garbage_collection}, State) ->
  handle_non_ready_bees(),
  {noreply, State};
    
handle_info({clean_up_apps}, State) ->
  clean_up(),
  {noreply, State};    

handle_info({'EXIT',_Pid,normal}, State) ->
  {noreply, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
  io:format("Pid exited: ~p~n", [Pid]),
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
  {ok, State};
  
handle_info({bee_started_normally, #bee{commit_hash = Sha} = Bee, #app{name = AppName} = App}, State) ->
  % StartedBee#bee{commit_hash = Sha}, App#app{sha = Sha}
  ?LOG(debug, "app_event_handler got bee_started_normally: ~p, ~p", [Bee, App]),
  apps:transactional_save(fun() ->
    RealApp = apps:find_by_name(AppName),
    apps:save(RealApp#app{sha = Sha})
  end),
  bees:transactional_save(fun() ->
    RealBee = bees:find_by_id(Bee#bee.id),
    bees:save(RealBee#bee{lastresp_time = date_util:now_to_seconds()})
  end),
  ok = kill_other_bees(Bee),
  {noreply, State};


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

% Spawn a process to try to connect to the instance
spawn_update_bee_status(Bee, From, Nums) ->
  spawn(fun() ->
    BeeStatus = try_to_connect_to_new_instance(Bee, Nums),
    RealBee = bees:find_by_id(Bee#bee.id),
    Saved = bees:save(RealBee#bee{status = BeeStatus}),
    ?LOG(info, "spawn_update_bee_status: ~p result: ~p", [BeeStatus, Saved]),
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
  
% Update configuration for an application from a proplist of configuration details
update_app_configuration(ConfigProplist, App, State) ->
  DefaultStartCmd = config:search_for_application_value(default_app_command, "thin -p [[PORT]] -u [[USER]] -g [[GROUP]] -e production start", router),
  DefaultStopCmd = config:search_for_application_value(default_stop_command, "thin stop", router),
  
  StartCmd  = update_app_configuration_param(start_command, DefaultStartCmd, ConfigProplist, App),
  StopCmd   = update_app_configuration_param(stop_command, DefaultStopCmd, ConfigProplist, App),
  % TODO: Define app base...
  Name      = update_app_configuration_param(name, App#app.name, ConfigProplist, App),
  Url       = update_app_configuration_param(url, "", ConfigProplist, App),
  Sticky    = update_app_configuration_param(sticky, false, ConfigProplist, App),
  UpdatedAt = update_app_configuration_param(updated_at, date_util:now_to_seconds(), ConfigProplist, App),
  % Hostnames = update_app_configuration_param(hostname, Name, ConfigProplist, App),
  Timeout   = update_app_configuration_param(timeout, 3600, ConfigProplist, App),
  MaxInst   = update_app_configuration_param(max_instances, 2, ConfigProplist, App),
  MinInst   = update_app_configuration_param(min_instances, 1, ConfigProplist, App),
    
  NewApp = App#app{
    start_command = StartCmd, stop_command = StopCmd, url = Url, name = Name, 
    updated_at = UpdatedAt,
    timeout = misc_utils:to_integer(Timeout), 
    sticky = Sticky,
    max_instances = misc_utils:to_integer(MaxInst),
    min_instances = misc_utils:to_integer(MinInst)
  },
  
  apps:create(NewApp),
  State.

% Get the current application configuration or the default
update_app_configuration_param(Param, Default, ConfigProplist, App) ->
  PropList = ?rec_info(app, App),
  % Get the value on the current application 
  CurrentVal = proplists:get_value(Param, PropList),
  case CurrentVal of
    undefined -> config:get_or_default(Param, Default, ConfigProplist);
    V -> V
  end.

% Add an application based on it's proplist
add_application_by_configuration(ConfigProplist, State) ->
  update_app_configuration(ConfigProplist, #app{}, State).

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

% TODO: reimplement?
% load_static_configs() ->
  % {ok, Files} = file:list_dir(?CONFIGS_DIR),
  % lists:map(fun(Filename) ->
  %   case filename:extension(Filename) of
  %     ".yaml" -> load_app_config_from_yaml_file(filename:join(?CONFIGS_DIR, Filename), ".yaml");
  %     _ ->
  %       ok
  %   end
  % end, Files).

% load_app_config_from_yaml_file(Filepath, Ext) ->
%   O1 = yaml:parse_file(Filepath),
%   O = misc_utils:atomize(O1, []),
%   Name = case proplists:is_defined(name, O) of
%     true  -> proplists:get_value(name, O);
%     false -> filename:basename(Filepath, Ext)
%   end,
%   update_app_configuration(O, #app{name = Name}, #state{}),
%   ok.

% MAINTENANCE
ping_bees() ->
  ReadyBees = lists:filter(fun(B) -> B#bee.status =:= ready end, bees:all()),
  lists:map(fun(B) ->
    spawn_update_bee_status(B, self(), 10)
  end, ReadyBees),
  ok.
  
% MAINTAIN THE ETS TABLES
flush_old_processes() ->
  lists:map(fun({Pid, App, Time}) ->
    case date_util:now_to_seconds() - Time > ?ACTION_TIMEOUT of
      false -> ok;
      true ->
        ets:delete(?UPDATERS_PID_TO_APP, Pid),
        ets:delete(?UPDATERS_APP_TO_PID, App)
    end
  end, ets:tab2list(?UPDATERS_PID_TO_APP)),
  lists:map(fun({Pid, App, Time}) ->
    case date_util:now_to_seconds() - Time > ?ACTION_TIMEOUT of
      false -> ok;
      true ->
        ets:delete(?LAUNCHERS_PID_TO_APP, Pid),
        ets:delete(?LAUNCHERS_APP_TO_PID, App)
    end
  end, ets:tab2list(?LAUNCHERS_PID_TO_APP)),
  ok.

% GARBAGE COLLECTION
handle_non_ready_bees() ->
  TerminatedBees = lists:filter(fun(B) -> B#bee.status =:= terminated end, bees:all()),
  ?LOG(debug, "garbage_collection on terminated: ~p", [TerminatedBees]),
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
  ?QSTORE:delete_queue(?WAIT_DB, B#bee.app_name),
  bees:delete(B);
cleanup_bee(B) ->
  (catch app_manager:request_to_terminate_bee(B)),
  ?QSTORE:delete_queue(?WAIT_DB, B#bee.app_name),
  bees:delete(B).

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
    [{_App, _Pid, Time}] -> 
      ?LOG(info, "Cannot launch app as there is already one in progress (timeout: ~p)", [date_util:now_to_seconds() - Time]),
      ok;
    _ -> 
    case node_manager:get_next_available(node) of
      false -> false;
      Host ->
        Node = node(Host),
        case App#app.type of
          static -> ok;
          _T -> spawn_to_start_new_instance(App, Node)
        end
      end
  end.
  
% Start with the app_launcher_fsm
spawn_to_start_new_instance(App, Host) when is_record(App, app) ->
  case App#app.sha of
    undefined ->
      ?NOTIFY({app, app_not_squashed, App});
    Sha ->
      {ok, P} = app_launcher_fsm:start_link(App, Host, Sha),
      Now = date_util:now_to_seconds(),
      app_launcher_fsm:launch(P, self()),
      ets:insert(?LAUNCHERS_APP_TO_PID, {App, P, Now}),
      ets:insert(?LAUNCHERS_PID_TO_APP, {P, App, Now})
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
