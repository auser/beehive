%%%-------------------------------------------------------------------
%%% File    : app_manager.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Oct 25 23:54:22 PDT 2009
%%%-------------------------------------------------------------------

-module (app_manager).

-include ("router.hrl").
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
  get_app_instance/1,
  handle_forwarding/3,
  proxy_terminated/1,
  ensure_minimum_backends_running/2
]).
        
-export ([
  mark_instance_ready/1,
  mark_instance_broken/1,
  mark_instance_busy/1
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

get_app_instance(Hostname) ->
  gen_server:call(?SERVER, {get_next_available_instance_of_app, Hostname}).

proxy_terminated(Pid) -> 
  gen_server:cast(?SERVER, {proxy_terminated, Pid}).

handle_forwarding(Hostname, Req, Body) ->
  gen_server:cast(?SERVER, {route_forward_request, Hostname, Req, Body}).
  
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
  % App store will store the application configuration in the format
  % {Subdomain, {app, record}},{Subdomain2, {app, record_2}}
  ?KVSTORE:start_link(?APP_DB),
  
  % Load the applications 
  load_static_configs(),
  
  % Try to make sure the pending backends are taken care of by either turning them broken for ready
  % timer:send_interval(timer:seconds(5), {manage_pending_backends}),
  % Run maintenance
  % timer:send_interval(timer:minutes(1), {garbage_collection}),
  % timer:send_interval(timer:seconds(10), {clean_up}),
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
  ?KVSTORE:delete(?APP_DB, AppName),
  {reply, ok, State};
    
% Show the status of the application instances for each app
handle_call({status}, _From, #state{app_dir = _AppDir} = State) ->
  Apps = ?KVSTORE:all(?APP_DB),
  Reply = [
    {apps, Apps}
  ],
  {reply, Reply, State};
  
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
handle_cast({terminate_all}, State) ->
  ?LOG(info, "Terminating all apps", []),
  lists:map(fun(_Name, Backends) ->
    lists:map(fun(Backend) -> stop_instance(Backend, State) end, Backends)
    end, app_srv:all(instances)),
  {reply, ok, State};
  
% Terminate all the instances of a certain application
handle_cast({terminate_app_instances, AppName}, State) ->
  Backends = app_srv:lookup(instances, AppName),
  lists:map(fun(Backend) -> stop_instance(Backend, State) end, Backends),
  app_srv:store(instances, AppName, []),
  {noreply, State};

handle_cast({proxy_terminated, Pid}, #state{active_apps = ActiveApps} = State) ->
  case proplists:is_defined(Pid, ActiveApps) of
    false -> 
      {noreply, State}; % not sure how it could get here... but just in case
    true ->
      Backend = proplists:get_value(Pid, ActiveApps),
      mark_instance_ready(Backend),
      {noreply, State}
  end;

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

handle_info({ensure_minimum_backends_running}, State) ->
  lists:map(fun(App) ->
    MinBackends = App#app.min_instances,
    RunningBackends = app_srv:get_host(App#app.name),
    if
      MinBackends > length(RunningBackends) ->
        ensure_minimum_backends_running1(length(RunningBackends), App, State),
        ?LOG(info, "Have to start a new instance for: ~p", [App#app.name]);
        true -> ok
    end
  end, apps:all(apps)),
  {noreply, State};

handle_info({manage_pending_backends}, State) ->
  PendingBackends = lists:filter(fun(B) -> B#backend.status == pending end, apps:all(backends)),
  lists:map(fun(B) ->
      BackendStatus = try_to_connect_to_new_instance(B, 10),
      ?LOG(info, "Marking ~p instance ~p", [B#backend.app_name, BackendStatus]),
      app_srv:update_backend_status(B, BackendStatus)
    % lists:map(fun(B) ->
      % ?LOG(info, "Garbage cleaning up on: ~p", [Backends#backend.app_name])
    % end, Backends)
  end, PendingBackends),
  {noreply, State};

handle_info({garbage_collection}, State) ->
  lists:map(fun(_Backends) ->
    ok
    % lists:map(fun(B) ->
      % ?LOG(info, "Garbage cleaning up on: ~p", [Backends#backend.app_name])
    % end, Backends)
  end, apps:all(backends)),
  
  {noreply, State};
  
handle_info({check_for_apps}, State) ->
  {noreply, State};
  
handle_info({'EXIT',_Pid,normal}, State) ->
  {noreply, State};

handle_info(Info, State) ->
  ?LOG(info, "Got info in app_srv: ~p", [Info]),
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

% Start the minimum number of instances for the instances
ensure_minimum_backends_running(App, State) ->
  Backends = apps:all(backends),
  ensure_minimum_backends_running1(length(Backends), App, State).
ensure_minimum_backends_running1(Count, #app{min_instances = MinBackends}, State) when Count >= MinBackends -> State;
ensure_minimum_backends_running1(Count, App, State) -> 
  ensure_minimum_backends_running1(Count + 1, start_new_instance(App, State), State).
    
% Start a new instance of the application
start_new_instance(App, State) ->
  Host = next_available_host(State),
  Port = next_available_port(State),
  
  TemplateCommand = App#app.start_command,
  
  RealCmd = template_command_string(TemplateCommand, [
                                                        {"[[PORT]]", misc_utils:to_list(Port)},
                                                        {"[[GROUP]]", App#app.group},
                                                        {"[[USER]]", App#app.user}
                                                      ]),
  % START INSTANCE
  % P = spawn(fun() -> os:cmd(NewCmd) end),
  % port_handler:start("thin -R beehive.ru --port 5000 start", "/Users/auser/Development/erlang/mine/router/test/fixtures/apps").
  ?LOG(info, "Starting on port ~p as ~p:~p with ~p", [Port, App#app.group, App#app.user, RealCmd]),
  Pid = port_handler:start(RealCmd, App#app.path),
  
  Backend  = #backend{
    app_name                = App#app.name,
    host                    = Host,
    port                    = Port,
    status                  = pending,
    pid                     = Pid,
    start_time              = date_util:now_to_seconds()
  },
  
  % apps:store(backend, App#app.name, Backend),
  app_srv:add_backend(Backend),
  
  % Let the instance know it's ready after it connects
  spawn(fun() ->
    BackendStatus = try_to_connect_to_new_instance(Backend, 10),
    ?LOG(info, "Marking ~p instance ~p", [Backend#backend.app_name, BackendStatus]),
    app_srv:update_backend_status(Backend, BackendStatus)
  end),
  
  ?LOG(info, "Spawned a new instance", []),
  App.

% Try to connect to the application instance while it's booting up
try_to_connect_to_new_instance(_Backend, 0) -> broken;
try_to_connect_to_new_instance(Backend, Attempts) ->
  case gen_tcp:connect(Backend#backend.host, Backend#backend.port, [binary, {packet, 0}]) of
    {ok, Sock} ->
      gen_tcp:close(Sock),
      ready;
    _ -> 
      timer:sleep(200),
      try_to_connect_to_new_instance(Backend, Attempts - 1)
  end.
  
% Get the next available port
next_available_port(_State) ->
  Backends = apps:all(backends),
  MyIp = host:myip(),
  
  LocalBackends = lists:filter(fun(AppBackends) ->
    AppBackends#backend.host =:= MyIp
  end, Backends),
  
  AllAppProplistsOfPorts = lists:map(fun({_AppName, AppBackends}) ->
    lists:flatten(lists:map(fun(Backend) -> Backend#backend.port end, AppBackends))
  end, LocalBackends),
  UsedPorts1 = lists:flatten(lists:map(fun({_Name, Ports}) -> Ports end, AllAppProplistsOfPorts)),
  UsedPorts = lists:filter(fun(Port) ->
    ?LOG(info, "Filtering port: ~p", [Port]),
    case Port of
      undefined -> false;
      P when is_integer(P) -> true;
      _ -> false
    end
  end, UsedPorts1),
  
  ?LOG(info, "UsedPorts: ~p", [UsedPorts]),
  NewPort = case misc_utils:max(UsedPorts) of
    0 -> ?STARTING_PORT;
    E -> E + 1
  end,
  NewPort.

next_available_host(_State) -> {127,0,0,1}.

% Update configuration for an application from a proplist of configuration details
update_app_configuration(ConfigProplist, App, State) ->
  DefaultStartCmd = apps:search_for_application_value(default_app_command, "thin -p [[PORT]] -u [[USER]] -g [[GROUP]] -e production start", router),
  DefaultStopCmd = apps:search_for_application_value(default_stop_command, "thin stop", router),
  
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
  ?LOG(info, "New app: ~p", [NewApp]),
  
  apps:store(app, Name, NewApp),
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
  ?LOG(info, "Adding an application: ~p~n", [ConfigProplist]),
  update_app_configuration(ConfigProplist, #app{}, State).

% kill the instance of the application
stop_instance(Backend, #state{dead_apps = DeadApps} = State) ->
  App = app_srv:lookup(app, Backend#backend.app_name),
  RealCmd = template_command_string(App#app.stop_command, [
                                                        {"[[PORT]]", erlang:integer_to_list(Backend#backend.port)},
                                                        {"[[GROUP]]", App#app.group},
                                                        {"[[USER]]", App#app.user}
                                                      ]),
                                                      
  Backend#backend.pid ! {stop, RealCmd},
  NewDeadApps = [App|DeadApps],
  
  RunningBackends1 = app_srv:lookup(instances, Backend#backend.app_name),
  RunningBackends = lists:delete(Backend, RunningBackends1),
  
  apps:store(backend, Backend#backend.app_name, RunningBackends),
  State#state{dead_apps = NewDeadApps}.

% Clean up applications
clean_up(State) ->
  Backends = app_srv:all(instances),
  lists:map(fun(Name, AppBackends) -> 
    App = app_srv:lookup(app, Name),
    clean_up_instances(AppBackends, App, State) 
  end, Backends).
  
% Clean up the instances
clean_up_instances([], _, State) -> State;
clean_up_instances([Backend|Rest], App, State) -> clean_up_instances(Rest, App, clean_up_instance(Backend, App, State)).

% Cleanup a single instance
clean_up_instance(Backend, App, State) ->
  % If the instance of the application has been used before
  case Backend#backend.lastresp_time of
    undefined -> State;
    _Time ->  clean_up_on_app_timeout(Backend, App, State)
  end.
  
% If the instance is not busy, the timeout has been exceeded and there are other application instances running
clean_up_on_app_timeout(#backend{status=Status,lastresp_time=LastReq} = Backend, #app{name=Name,timeout=Timeout,min_instances=Min} = App, State) ->
	NumBackends = length(app_srv:lookup(instance, Name)),
	TimeDiff = date_util:time_difference_from_now(LastReq),
  % ?LOG(info, "clean_up_on_app_timeout: ~p > ~p, ~p > ~p", [NumBackends, App#app.min_instances, TimeDiff, Timeout]),
  if
    Status =/= busy andalso NumBackends > Min andalso TimeDiff > Timeout -> stop_instance(Backend, State);
    true -> clean_up_on_broken_status(Backend, App, State)
  end.
  
% If the instance status is broken, then stop the app
clean_up_on_broken_status(Backend, App, State) ->
  % ?LOG(info, "clean_up_on_broken_status: ~p", [Backend#backend.status]),
  if
    Backend#backend.status =:= broken -> stop_instance(Backend, State);
    true -> clean_up_on_busy_and_stale_status(Backend, App, State)
  end.
  
% If the instance is busy, but hasn't served a request in a long time, kill it
clean_up_on_busy_and_stale_status(#backend{status = Status, lastresp_time = LastReq} = Backend, #app{timeout = Timeout} = App, State) ->
	TimeDiff = date_util:time_difference_from_now(LastReq),
  % ?LOG(info, "clean_up_on_busy_and_stale_status: ~p > ~p + ~p", [TimeDiff, Timeout, ?TIME_BUFFER]),
  if
    Status =:= busy andalso TimeDiff > Timeout + ?TIME_BUFFER -> 
			stop_instance(Backend, State);
    true -> clean_up_on_long_running_instance(Backend, App, State)
  end.

% If the application has been running for a while, kill it
clean_up_on_long_running_instance(#backend{start_time = StartTime} = Backend, _App, State) ->
	TimeDiff = date_util:time_difference_from_now(StartTime),
  % ?LOG(info, "clean_up_on_long_running_instance: ~p > ~p", [TimeDiff, ?RUN_INSTANCE_TIME_PERIOD]),
  if
    TimeDiff > ?RUN_INSTANCE_TIME_PERIOD -> stop_instance(Backend, State);
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

% turn the command string from the comand string with the values
% of [[KEY]] replaced by the corresponding proplist element of
% the format:
%   {[[PORT]], "80"}
template_command_string(OriginalCommand, []) -> OriginalCommand;
template_command_string(OriginalCommand, [{Str, Replace}|T]) ->
  NewCommand = string_utils:gsub(OriginalCommand, Str, Replace),
  template_command_string(NewCommand, T).

% Turn the priplists into atoms
atomize([], Acc) -> Acc;
atomize([{K,V}|Rest], Acc) -> atomize(Rest, [{misc_utils:to_atom(K), V}|Acc]).

% Mark this instance as busy
mark_instance_busy(Backend)   -> mark_instance_status(Backend, busy).
% Mark instance as broken
mark_instance_broken(Backend) -> mark_instance_status(Backend, broken).
% Mark this instance as ready
mark_instance_ready(Backend)  -> mark_instance_status(Backend, ready).
    
% Mark an instance of an app as the denoted status
mark_instance_status(Backend, Status) ->
  NewBackend = Backend#backend{status = Status, lastresp_time = date_util:now_to_seconds()},
  apps:store(backend, Backend#backend.app_name, [NewBackend]),
  NewBackend.