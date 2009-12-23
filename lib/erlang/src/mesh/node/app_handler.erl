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
  start_new_instance/4,
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
  
start_new_instance(App, Sha, AppLauncher, From) ->
  gen_server:call(?SERVER, {start_new_instance, App, Sha, AppLauncher, From}).

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
handle_call({start_new_instance, App, Sha, AppLauncher, From}, _From, #state{
                                      available_ports = AvailablePorts} = State) ->
  Port = case AvailablePorts of
    [] -> ?STARTING_PORT;
    [P|_] -> P
  end,
  
  % Then start it :)
  ?LOG(debug, "internal_start_new_instance: ~p, ~p, ~p, ~p, ~p~n", [App, Sha, Port, AppLauncher, From]),
  internal_start_new_instance(App, Sha, Port, AppLauncher, From),
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
internal_start_new_instance(App, Sha, Port, AppLauncher, _From) ->
  case find_and_transfer_bee(App, Sha) of
    {ok, Node, LocalPath} ->
      Proplists = [{sha, Sha}, {port, Port}, {bee_image, LocalPath}, {storage_node, Node}],
      initialize_application(App, Proplists, AppLauncher);
    E -> 
      io:format("Error: ~p~n", [E]),
      E
  end.

% Initialize the node
initialize_application(App, PropLists, AppLauncher) ->
  Sha = proplists:get_value(sha, PropLists),
  Port = proplists:get_value(port, PropLists),
  ImagePath = proplists:get_value(bee_image, PropLists),
  StorageNode = proplists:get_value(storage_node, PropLists),
  
  Host = host:myip(),
  Id = {App#app.name, Host, Port},
  StartedAt = date_util:now_to_seconds(),
  
  {Proplist, Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("mount-and-start-bee", [
    {"[[HOST_IP]]", Host},
    {"[[BEE_IMAGE]]", ImagePath},
    {"[[PORT]]", misc_utils:to_list(Port)},
    {"[[SHA]]", Sha},
    {"[[START_TIME]]", StartedAt},
    {"[[APP_NAME]]", App#app.name}
  ]),
  
  AppRootPath = proplists:get_value(dir, Proplist),
  
  Bee  = #bee{
    id                      = Id,
    app_name                = App#app.name,
    host                    = Host,
    host_node               = node(self()),
    storage_node            = StorageNode,
    path                    = AppRootPath,
    port                    = Port,
    status                  = pending,
    pid                     = self(),
    start_time              = StartedAt
  },
  
  % Store the app in the local ets table
  ets:insert(?TAB_ID_TO_BEE, {Id, Bee}),
  ets:insert(?TAB_NAME_TO_BEE, {App#app.name, Bee}),
  
  case Status of
    0 ->
      AppLauncher ! {started_bee, Bee},
      Bee;
    Code ->
      AppLauncher ! {error, Code}
  end.

% Find and transfer the bee
find_and_transfer_bee(App, Sha) ->
  Nodes = lists:map(fun(N) -> node(N) end, node_manager:get_storage()),
  Path = next_free_honeycomb(App),
  LocalPath = filename:join([filename:absname(""), lists:append([Path, "/", "app.squashfs"])]),
  case find_bee_on_storage_nodes(App, Sha, Nodes) of
    {ok, Node, RemotePath} ->
      slugger:get(Node, RemotePath, LocalPath),
      {ok, Node, LocalPath};
    E -> E
  end.

% Look on the node and see if it has the 
find_bee_on_storage_nodes(App, _Sha, []) -> 
  % ?NOTIFY({app, app_not_squashed, Name}),
  io:format("App not found: ~p~n", [App]),
  ?NOTIFY({app, updated, App}),
  {error, not_found};
find_bee_on_storage_nodes(App, Sha, [Node|Rest]) ->
  case rpc:call(Node, ?STORAGE_SRV, has_squashed_repos, [App, Sha]) of
    false -> find_bee_on_storage_nodes(App, Sha, Rest);
    Path -> {ok, Node, Path}
  end.

% kill the instance of the application  
internal_stop_instance(#bee{commit_hash = Sha} = Bee, App, From) when is_record(App, app) ->
  io:format("internal_stop_instance(~p)~n", [Bee]),
  Pid = Bee#bee.pid,
  
  Pid ! {stop},
  
  io:format("internal_stop_instance: ~p and ~p~n", [Sha, App#app.name]),
  case Sha of
    undefined -> ok;
    _E ->
      StopProplists = [{"[[SHA]]", Sha},{"[[APP_NAME]]", App#app.name}],
      ?TEMPLATE_SHELL_SCRIPT_PARSED("stop-bee", StopProplists)
  end,
  
  case ets:lookup(?TAB_ID_TO_BEE, {App#app.name, Bee#bee.host, Bee#bee.port}) of
    [{Key, _B}] ->
      ets:delete(?TAB_NAME_TO_BEE, App#app.name),
      ets:delete(?TAB_ID_TO_BEE, Key);
    _ -> true
  end,
  From ! {bee_terminated, Bee}.

% Get a new honeycomb location for the new bee
next_free_honeycomb(App) ->
  BaseDir = config:search_for_application_value(squashed_storage, ?BH_RELATIVE_DIR("apps"), storage),
  UniqueName = apps:build_on_disk_app_name(App),
  {Proplists, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("next-free-honeycomb", [
    {"[[APP_NAME]]", App#app.name},
    {"[[SLOT_DIR]]", md5:hex(UniqueName)},
    {"[[DESTINATION]]", BaseDir}
  ]),
  proplists:get_value(dir, Proplists).
  
handle_pid_exit(_Pid, _Reason, State) ->
  State.