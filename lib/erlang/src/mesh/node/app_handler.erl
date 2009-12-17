%%%-------------------------------------------------------------------
%%% File    : app_handler.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Nov 19 12:45:16 PST 2009
%%%-------------------------------------------------------------------

-module (app_handler).
-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop/0,
  start_new_instance/3,
  stop_instance/3, stop_app/2,
  can_deploy_new_app/0,
  has_app_named/1
]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  max_bees,             % maximum number of bees on this host
  available_ports          % available ports on this node
}).
-define(SERVER, ?MODULE).

-define (STORAGE_SRV, bh_storage_srv).

-define (TAB_ID_TO_BEE, 'id_to_bee_table').
-define (TAB_NAME_TO_BEE, 'name_to_bee_table').
-define (TAB_PID_TO_BEE, 'port_to_bee_table').

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, {stop}).
  
can_deploy_new_app() ->
  gen_server:call(?SERVER, {can_deploy_new_app}).
  
start_new_instance(App, AppLauncher, From) ->
  gen_server:call(?SERVER, {start_new_instance, App, AppLauncher, From}).

stop_instance(Bee, App, From) ->
  gen_server:call(?SERVER, {stop_instance, Bee, App, From}).

has_app_named(Name) ->
  gen_server:call(?SERVER, {has_app_named, Name}).
  
stop_app(App, From) ->
  gen_server:cast(?SERVER, {stop_app, App, From}).

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
  
  Opts = [named_table, set],
  ets:new(?TAB_ID_TO_BEE, Opts),
  ets:new(?TAB_PID_TO_BEE, Opts),
  ets:new(?TAB_NAME_TO_BEE, Opts),
  
  MaxBackends     = ?MAX_BACKENDS_PER_HOST,
  % set a list of ports that the node can use to deploy applications
  AvailablePorts  = lists:seq(?STARTING_PORT, ?STARTING_PORT + MaxBackends),
  
  {ok, #state{
    max_bees = MaxBackends,
    available_ports = AvailablePorts
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
handle_call({start_new_instance, App, AppLauncher, From}, _From, #state{
                                      available_ports = AvailablePorts} = State) ->
  Port = case AvailablePorts of
    [] -> ?STARTING_PORT;
    [P|_] -> P
  end,
  % first, stop all the bees that are running with this app name
  AppBees = ets:match(?TAB_NAME_TO_BEE, {App#app.name, '_'}),
  
  case AppBees of
    [] -> ok;
    _ ->
      lists:map(fun(Bee) -> internal_stop_instance(Bee, App, From) end, AppBees)
  end,
  
  % Then start it :)
  internal_start_new_instance(App, Port, AppLauncher, From),
  NewAvailablePorts = lists:delete(Port, AvailablePorts),
  {reply, ok, State#state{
    available_ports = NewAvailablePorts
  }};

handle_call({stop_instance, Backend, App, From}, _From, #state{available_ports = AvailablePorts} = State) ->
  Port = Backend#bee.port,
  internal_stop_instance(Backend, App, From),
  NewAvailablePorts = [Port|AvailablePorts],
  {reply, ok, State#state{available_ports = NewAvailablePorts}};

handle_call({has_app_named, Name}, _From, State) ->
  Reply = case ets:lookup(?TAB_NAME_TO_BEE, Name) of
      [{Name, _Bee}] -> true;
      _ -> false
    end,
  {reply, Reply, State};

% Check if this node can deploy a new application or not
handle_call({can_deploy_new_app}, _From, #state{max_bees = Max} = State) ->
  Curr = ets:match(?TAB_NAME_TO_BEE, '$1'),
  Reply = (length(Curr) < Max),
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
% Good spot for optimization
handle_cast({stop_app, App, From}, State) ->
  AppBees = ets:match(?TAB_NAME_TO_BEE, {App#app.name, '_'}),
  
  io:format("AppBees: ~p~n", [AppBees]),
  lists:map(fun(Bee) ->
    internal_stop_instance(Bee, App, From)
  end, AppBees),
  
  NewAvailablePorts = lists:map(fun(B) -> B#bee.port end, AppBees),
  {noreply, State#state{available_ports = NewAvailablePorts}};
  
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG(info, "Pid exited: ~p because ~p", [Pid, Reason]),
  {noreply, handle_pid_exit(Pid, Reason, State)};
handle_info({port_closed, Pid, 0}, State) ->
  case find_pid_in_pid_table(Pid) of
    {Pid, Bee, _App, _From} ->
      ?NOTIFY({bee, cannot_connect, Bee}),
      ets:delete(?TAB_PID_TO_BEE, Pid),
      ets:delete(?TAB_ID_TO_BEE, Bee#bee.id);
    _ -> ok
  end,
  ?LOG(info, "Port closed: ~p", [Pid]),
  {noreply, State};
handle_info({data, _Data}, State) ->
  % io:format("Received data from a port: ~p~n", [Data]),
  {noreply, State};
handle_info(Info, State) ->
  ?LOG(info, "~p caught info: ~p", [?MODULE, Info]),
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
% Start new instance
internal_start_new_instance(App, Port, AppLauncher, From) ->
  case find_and_transfer_bee(App) of
    {ok, Node, LocalPath} ->
      case mount_bee_from_path(App, LocalPath) of
        {ok, Path} ->
          io:format("Starting bee in: ~p~n", [Path]),
          case run_application_on_port_in_path(App, Port, Path, From) of
            Bee when is_record(Bee, bee) ->
              AppLauncher ! {started_bee, Bee},
              Bee#bee{storage_node = Node};
            E -> E
          end;
        E -> E
      end;
    E -> 
      io:format("Error: ~p~n", [E]),
      E
  end.

% Find and transfer the bee
find_and_transfer_bee(App) ->
  Nodes = lists:map(fun(N) -> node(N) end, node_manager:get_storage()),
  Path = next_free_honeycomb(App),
  io:format("next_free_honeycomb: ~p~n", [Path]),
  LocalPath = filename:join([filename:absname(""), lists:append([Path, "/", "app.squashfs"])]),
  case find_bee_on_storage_nodes(App, Nodes) of
    {ok, Node, RemotePath} ->
      slugger:get(Node, RemotePath, LocalPath),
      {ok, Node, LocalPath};
    E -> E
  end.

% Look on the node and see if it has the 
find_bee_on_storage_nodes(App, []) -> 
  % ?NOTIFY({app, app_not_squashed, Name}),
  io:format("App not found: ~p~n", [App]),
  {ok, P} = app_updater_fsm:start_link(App),
  app_updater_fsm:go(P, self()),
  {error, not_found};
find_bee_on_storage_nodes(App, [Node|Rest]) ->
  case rpc:call(Node, ?STORAGE_SRV, has_squashed_repos, [App#app.name]) of
    false -> find_bee_on_storage_nodes(App, Rest);
    Path -> {ok, Node, Path}
  end.

% Mount the bee
mount_bee_from_path(App, ImagePath) ->
  io:format("mount_bee_from_path(~p, ~p)~n", [App#app.name, ImagePath]),
  {Proplist, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("mount-bee", [
    {"[[APP_NAME]]", App#app.name},
    {"[[BEE_IMAGE]]", ImagePath}
  ]),
  io:format("Mount bee: ~p~n", [Proplist]),
  case proplists:get_value(path, Proplist) of
    undefined -> {error, not_mounted};
    Path ->
      {ok, Path}
  end.

% Start a new instance of the application
run_application_on_port_in_path(App, Port, AppRootPath, From) ->  
  Host = host:myip(),
  Id = {App#app.name, Host, Port},
  StartedAt = date_util:now_to_seconds(),
  
  Tempfile = misc_utils:create_templated_tempfile("start-bee", [
    {"[[PORT]]", misc_utils:to_list(Port)},
    {"[[APP_HOME]]", AppRootPath},
    {"[[START_TIME]]", StartedAt},
    {"[[APP_NAME]]", App#app.name}
  ]),
  
  RealCmd = lists:append(["/bin/bash ", misc_utils:to_list(Tempfile)]),
  io:format("RealCmd: ~p~n", [RealCmd]),
  Pid = port_handler:start(RealCmd, AppRootPath, self(), [stderr_to_stdout]), % stderr_to_stdout
  
  io:format("Started port_handler: ~p~n", [Pid]),
  Bee  = #bee{
    id                      = Id,
    app_name                = App#app.name,
    host                    = Host,
    host_node               = node(self()),
    path                    = AppRootPath,
    port                    = Port,
    status                  = pending,
    pid                     = Pid,
    start_time              = StartedAt
  },
  
  % Store the app in the local ets table
  ets:insert(?TAB_ID_TO_BEE, {Id, Bee}),
  ets:insert(?TAB_NAME_TO_BEE, {App#app.name, Bee}),
  ets:insert(?TAB_PID_TO_BEE, {Pid, Bee, App, From}),
  Bee.

% kill the instance of the application  
internal_stop_instance(Bee, App, From) when is_record(App, app) ->
  io:format("internal_stop_instance(~p)~n", [Bee]),
  Port = Bee#bee.port,
  Pid = Bee#bee.pid,
  AppRootPath = Bee#bee.path,
  
  Pid ! {stop},
  
  {Proplist, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("stop-bee", [
    {"[[PORT]]", misc_utils:to_list(Port)},
    {"[[APP_HOME]]", AppRootPath},
    {"[[APP_NAME]]", App#app.name}
  ]),
  
  % {_AnotherProplist, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("unmount-bee", [
  %   {"[[APP_HOME]]", AppRootPath},
  %   {"[[APP_NAME]]", App#app.name}
  % ]),
  
  io:format("Proplist: ~p~n", [Proplist]),
  
  case ets:lookup(?TAB_ID_TO_BEE, {App#app.name, Bee#bee.host, Bee#bee.port}) of
    [{Key, _B}] ->
      ets:delete(?TAB_NAME_TO_BEE, App#app.name),
      ets:delete(?TAB_PID_TO_BEE, Bee#bee.pid),
      ets:delete(?TAB_ID_TO_BEE, Key);
    _ -> true
  end,
  From ! {bee_terminated, Bee}.

% Handle pid exiting
handle_pid_exit(Pid, _Code, #state{available_ports = AvailablePorts} = State) ->
  case find_pid_in_pid_table(Pid) of
    false -> 
      State;
    {Bee, App, From} -> 
      NewPorts = lists:delete(Bee#bee.port, AvailablePorts),
      internal_stop_instance(Bee, App, From),
      State#state{available_ports = NewPorts}
  end.

% Look up the pid in the ets table
find_pid_in_pid_table(Port) ->
  case ets:lookup(?TAB_PID_TO_BEE, Port) of
    [{_Key, Bee, App, From}] ->
      {Bee, App, From};
    _ -> false
  end.

% Get a new honeycomb location for the new bee
next_free_honeycomb(App) ->
  BaseDir = config:search_for_application_value(squashed_storage, "/opt/beehive/apps", storage),
  UniqueName = apps:build_on_disk_app_name(App),
  {Proplists, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("next-free-honeycomb", [
    {"[[APP_NAME]]", App#app.name},
    {"[[SLOT_DIR]]", md5:hex(UniqueName)},
    {"[[DESTINATION]]", BaseDir}
  ]),
  proplists:get_value(dir, Proplists).