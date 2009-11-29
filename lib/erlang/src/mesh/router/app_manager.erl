%%%-------------------------------------------------------------------
%%% File    : app_manager.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Oct 25 23:54:22 PDT 2009
%%%-------------------------------------------------------------------

-module (app_manager).

-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export ([
  instance/0,
  status/0,
  terminate_all/0,
  terminate_app_instances/1,
  add_application/1,
  spawn_update_bee_status/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  app_dir,              % Default directory of applications
  active_apps = [],     % Active applications (that have their socket handled via proxy_handler)
  dead_apps = []        % Apps that have been killed and or are dead
}).
-define (SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
status() -> gen_server:call(?SERVER, {status}).
instance() -> whereis(?SERVER).

terminate_all() ->
  ?LOG(info, "Terminate all instances~n", []),
  gen_server:cast(?SERVER, {terminate_all}).
  
terminate_app_instances(Appname) ->
  ?LOG(info, "Terminate all instances of this application~n", []),
  gen_server:cast(?SERVER, {terminate_app_instances, Appname}).
  
add_application(ConfigProplist) ->
  gen_server:call(?SERVER, {add_application_by_configuration, ConfigProplist}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
  % Load the applications
  load_static_configs(),
  
  % Try to make sure the pending bees are taken care of by either turning them broken or ready
  % timer:send_interval(timer:seconds(5), {manage_pending_bees}),
  % Run maintenance
  timer:send_interval(timer:seconds(20), {ping_bees}),
  timer:send_interval(timer:minutes(10), {garbage_collection}),
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
%   lists:map(fun(_Name, Backends) ->
%     lists:map(fun(Backend) -> app_handler:stop_instance(Backend, State) end, Backends)
%     end, bee_srv:all(instances)),
%   {reply, ok, State};
  
% Terminate all the instances of a certain application
% handle_cast({terminate_app_instances, AppName}, State) ->
%   Backends = bee_srv:lookup(instances, AppName),
%   lists:map(fun(Backend) -> app_handler:stop_instance(Backend, State) end, Backends),
%   bee_srv:store(instances, AppName, []),
%   {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------  
handle_info({clean_up}, State) ->
  {noreply, clean_up(State)};

handle_info({manage_pending_bees}, State) ->
  PendingBackends = lists:filter(fun(B) -> B#bee.status == pending end, apps:all(bees)),
  lists:map(fun(B) ->
      BackendStatus = try_to_connect_to_new_instance(B, 10),
      bee_srv:update_bee_status(B, BackendStatus)
    % lists:map(fun(B) ->
      % ?LOG(info, "Garbage cleaning up on: ~p", [Backends#bee.app_name])
    % end, Backends)
  end, PendingBackends),
  {noreply, State};

handle_info({ping_bees}, State) ->
  ping_bees(),
  {noreply, State};

handle_info({garbage_collection}, State) ->
  handle_non_ready_bees(),
  {noreply, State};
    
handle_info({'EXIT',_Pid,normal}, State) ->
  {noreply, State};

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
  terminate_all(),
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
spawn_update_bee_status(Backend, From, Nums) ->
  spawn(fun() ->
    BackendStatus = try_to_connect_to_new_instance(Backend, Nums),
    bees:update(Backend#bee{status = BackendStatus}),
    From ! {updated_bee_status, BackendStatus}
  end).

% Try to connect to the application instance while it's booting up
try_to_connect_to_new_instance(_Backend, 0) -> broken;
try_to_connect_to_new_instance(Backend, Attempts) ->
  case gen_tcp:connect(Backend#bee.host, Backend#bee.port, [binary, {packet, 0}]) of
    {ok, Sock} ->
      gen_tcp:close(Sock),
      ready;
    _ -> 
      timer:sleep(200),
      try_to_connect_to_new_instance(Backend, Attempts - 1)
  end.
  
% Update configuration for an application from a proplist of configuration details
update_app_configuration(ConfigProplist, App, State) ->
  DefaultStartCmd = config:search_for_application_value(default_app_command, "thin -p [[PORT]] -u [[USER]] -g [[GROUP]] -e production start", beehive),
  DefaultStopCmd = config:search_for_application_value(default_stop_command, "thin stop", beehive),
  
  StartCmd  = update_app_configuration_param(start_command, DefaultStartCmd, ConfigProplist, App),
  StopCmd   = update_app_configuration_param(stop_command, DefaultStopCmd, ConfigProplist, App),
  % TODO: Define app base...
  Name      = update_app_configuration_param(name, App#app.name, ConfigProplist, App),
  Path      = update_app_configuration_param(path, "./", ConfigProplist, App),
  Url       = update_app_configuration_param(url, "", ConfigProplist, App),
  UpdatedAt = update_app_configuration_param(updated_at, date_util:now_to_seconds(), ConfigProplist, App),
  CC        = update_app_configuration_param(concurrency, 1, ConfigProplist, App),
  % Hostnames = update_app_configuration_param(hostname, Name, ConfigProplist, App),
  Timeout   = update_app_configuration_param(timeout, 30000, ConfigProplist, App),
  MaxInst   = update_app_configuration_param(max_instances, 2, ConfigProplist, App),
  MinInst   = update_app_configuration_param(min_instances, 1, ConfigProplist, App),
  User      = update_app_configuration_param(user, "beehive", ConfigProplist, App),
  Group     = update_app_configuration_param(group, "beehive", ConfigProplist, App),
  
  NewApp = App#app{
    start_command = StartCmd, stop_command = StopCmd, url = Url, name = Name, 
    path = Path, updated_at = UpdatedAt,
    concurrency = misc_utils:to_integer(CC),
    timeout = misc_utils:to_integer(Timeout), 
    max_instances = misc_utils:to_integer(MaxInst),
    min_instances = misc_utils:to_integer(MinInst), 
    user = User, 
    group = Group
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
clean_up(State) ->
  Backends = bee_srv:all(instances),
  lists:map(fun(Name, AppBackends) -> 
    App = bee_srv:lookup(app, Name),
    clean_up_instances(AppBackends, App, State) 
  end, Backends).
  
% Clean up the instances
clean_up_instances([], _, State) -> State;
clean_up_instances([Backend|Rest], App, State) -> clean_up_instances(Rest, App, clean_up_instance(Backend, App, State)).

% Cleanup a single instance
clean_up_instance(Backend, App, State) ->
  % If the instance of the application has been used before
  case Backend#bee.lastresp_time of
    undefined -> State;
    _Time ->  clean_up_on_app_timeout(Backend, App, State)
  end.
  
% If the instance is not busy, the timeout has been exceeded and there are other application instances running
clean_up_on_app_timeout(#bee{status=Status,lastresp_time=LastReq} = Backend, #app{name=Name,timeout=Timeout,min_instances=Min} = App, State) ->
	NumBackends = length(bee_srv:lookup(instance, Name)),
	TimeDiff = date_util:time_difference_from_now(LastReq),
  % ?LOG(info, "clean_up_on_app_timeout: ~p > ~p, ~p > ~p", [NumBackends, App#app.min_instances, TimeDiff, Timeout]),
  if
    % stop_instance(Backend, App, From)
    Status =/= busy andalso NumBackends > Min andalso TimeDiff > Timeout -> app_handler:stop_instance(Backend, App, self());
    true -> clean_up_on_broken_status(Backend, App, State)
  end.
  
% If the instance status is broken, then stop the app
clean_up_on_broken_status(Backend, App, State) ->
  % ?LOG(info, "clean_up_on_broken_status: ~p", [Backend#bee.status]),
  if
    Backend#bee.status =:= broken -> app_handler:stop_instance(Backend, App, self());
    true -> clean_up_on_busy_and_stale_status(Backend, App, State)
  end.
  
% If the instance is busy, but hasn't served a request in a long time, kill it
clean_up_on_busy_and_stale_status(#bee{status = Status, lastresp_time = LastReq} = Backend, #app{timeout = Timeout} = App, State) ->
	TimeDiff = date_util:time_difference_from_now(LastReq),
  % ?LOG(info, "clean_up_on_busy_and_stale_status: ~p > ~p + ~p", [TimeDiff, Timeout, ?TIME_BUFFER]),
  if
    Status =:= busy andalso TimeDiff > Timeout + ?TIME_BUFFER -> 
			app_handler:stop_instance(Backend, App, self());
    true -> clean_up_on_long_running_instance(Backend, App, State)
  end.

% If the application has been running for a while, kill it
clean_up_on_long_running_instance(#bee{start_time = StartTime} = Backend, App, State) ->
	TimeDiff = date_util:time_difference_from_now(StartTime),
  % ?LOG(info, "clean_up_on_long_running_instance: ~p > ~p", [TimeDiff, ?RUN_INSTANCE_TIME_PERIOD]),
  if
    TimeDiff > ?RUN_INSTANCE_TIME_PERIOD -> app_handler:stop_instance(Backend, App, self());
    true -> State
  end.

load_static_configs() ->
  {ok, Files} = file:list_dir(?CONFIGS_DIR),
  lists:map(fun(Filename) ->
    case filename:extension(Filename) of
      ".yaml" -> load_app_config_from_yaml_file(filename:join(?CONFIGS_DIR, Filename), ".yaml");
      _ ->
        ok
    end
  end, Files).

load_app_config_from_yaml_file(Filepath, Ext) ->
  O1 = yaml:parse_file(Filepath),
  O = atomize(O1, []),
  Name = case proplists:is_defined(name, O) of
    true  -> proplists:get_value(name, O);
    false -> filename:basename(Filepath, Ext)
  end,
  update_app_configuration(O, #app{name = Name}, #state{}),
  ok.

% MAINTENANCE
ping_bees() ->
  ReadyBackends = lists:filter(fun(B) -> B#bee.status =:= ready end, bees:all()),
  lists:map(fun(B) ->
    spawn_update_bee_status(B, self(), 10)
  end, ReadyBackends),
  ok.

% GARBAGE COLLECTION
handle_non_ready_bees() ->
  DownBackends = lists:filter(fun(B) -> B#bee.status =/= ready andalso B#bee.sticky =:= false end, bees:all()),
  lists:map(fun(B) ->
    spawn(fun() -> try_to_reconnect_to_bee(B, 5) end)
  end, DownBackends),
  ok.

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
      bees:update(B#bee{status = NewStatus})
  end.

% Cleanup the bee. Remove traces of the bee from the system
cleanup_bee(B) ->
  ?QSTORE:delete_queue(?WAIT_DB, B#bee.app_name),
  bees:delete(B).

% Turn the priplists into atoms
atomize([], Acc) -> Acc;
atomize([{K,V}|Rest], Acc) -> atomize(Rest, [{misc_utils:to_atom(K), V}|Acc]).