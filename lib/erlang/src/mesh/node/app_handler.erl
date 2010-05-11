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
-behaviour(gen_cluster).

%% API
-export([
  start_link/0,
  stop/0,
  start_new_instance/4,
  stop_instance/3, stop_app/2,
  can_deploy_new_app/0,
  has_app_named/1,
  seed_nodes/1
]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
% gen_cluster callback
-export([handle_join/3, handle_leave/4]).

-record(state, {
  max_bees             % maximum number of bees on this host
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
seed_nodes(_State) -> global:whereis_name(node_manager).

start_link() ->
  gen_cluster:start_link({local, ?SERVER}, ?SERVER, [], []).

stop() ->
  gen_cluster:call(?SERVER, {stop}).
  
can_deploy_new_app() ->
  gen_cluster:call(?SERVER, {can_deploy_new_app}).
  
start_new_instance(App, Sha, AppLauncher, From) ->
  gen_cluster:call(?SERVER, {start_new_instance, App, Sha, AppLauncher, From}).

stop_instance(Bee, App, From) ->
  gen_cluster:call(?SERVER, {stop_instance, Bee, App, From}).

has_app_named(Name) ->
  gen_cluster:call(?SERVER, {has_app_named, Name}).
  
stop_app(App, From) ->
  gen_cluster:cast(?SERVER, {stop_app, App, From}).

%%====================================================================
%% gen_cluster callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
  process_flag(trap_exit, true),
  
  Opts = [named_table, set],
  ets:new(?TAB_ID_TO_BEE, Opts),
  ets:new(?TAB_NAME_TO_BEE, Opts),
  
  MaxBackends     = ?MAX_BACKENDS_PER_HOST,
  
  {ok, #state{
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
handle_call({start_new_instance, App, Sha, AppLauncher, From}, _From, State) ->
  
  Port = bh_host:unused_port(),
  % Then start it :)
  ?LOG(debug, "internal_start_new_instance: ~p, ~p, ~p, ~p, ~p~n", [App, Sha, Port, AppLauncher, From]),
  internal_start_new_instance(App, Sha, Port, AppLauncher, From),
  {reply, ok, State};

handle_call({stop_instance, Backend, App, From}, _From, State) ->
  internal_stop_instance(Backend, App, From),
  {reply, ok, State};

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
handle_cast({stop_app, App, _From}, State) ->
  AppBees = lists:flatten(ets:match(?TAB_NAME_TO_BEE, {App#app.name, '$1'})),
  
  io:format("Terminating AppBees: ~p~n", [AppBees]),  
  {noreply, State};
  
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
handle_info({stop}, State) ->
  {stop, normal, State};
handle_info({port_exited,Port,Pid}, State) ->
  % Do something with this...
  io:format("Port process exited: ~p, ~p~n", [Port, Pid]),
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
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(_JoiningPid, _Pidlist, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_leave(LeavingPid, Pidlist, Info, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------
handle_leave(_LeavingPid, _Pidlist, _Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
% Start new instance
internal_start_new_instance(App, Sha, Port, AppLauncher, From) ->
  case find_and_transfer_bee(App, Sha) of
    {ok, Node, LocalPath} ->
      Proplists = [{sha, Sha}, {port, Port}, {bee_image, LocalPath}, {storage_node, Node}],
      case mount_application(App, Proplists) of
        {ok, _Other} ->
          initialize_application(App, Proplists, AppLauncher, From);
        Error -> Error
      end;
    E -> 
      io:format("Error: ~p~n", [E]),
      E
  end.

mount_application(App, PropLists) ->
  ScratchDisk = config:search_for_application_value(scratch_disk, ?BH_RELATIVE_DIR("tmp"), storage),
  RunDir = config:search_for_application_value(squashed_storage, ?BH_RELATIVE_DIR("run"), storage),
  UniqueName = apps:build_on_disk_app_name(App),

  WorkingDir = lists:flatten([ScratchDisk, "/", App#app.name]),
  MountedDir = lists:flatten([RunDir, "/", UniqueName]),
  BeeImage = proplists:get_value(bee_image, PropLists),
  Port = misc_utils:to_list(proplists:get_value(port, PropLists)),
  
  OtherOpts = [
    {working_directory, WorkingDir},
    {target_directory, MountedDir},
    {bee_image, BeeImage},
    {run_dir, RunDir},
    {port, Port}
  ],
  lists:map(fun(Dir) -> file:make_dir(Dir) end, [RunDir, ScratchDisk, WorkingDir, MountedDir]),
  EnvOpts = apps:build_app_env(App, OtherOpts),
  CmdOpts = lists:flatten([{cd, MountedDir}|EnvOpts]),
  
  erlang:display({cmd_opts, CmdOpts}),
  
  babysitter:run(App#app.template, mount, CmdOpts).

% Initialize the node
initialize_application(App, PropLists, AppLauncher, _From) ->
  erlang:display({initialize_application, PropLists}),
  Sha = proplists:get_value(sha, PropLists),
  Port = proplists:get_value(port, PropLists),
  ImagePath = proplists:get_value(bee_image, PropLists),
  StorageNode = proplists:get_value(storage_node, PropLists),
  
  ScratchDisk = config:search_for_application_value(scratch_disk, ?BH_RELATIVE_DIR("tmp"), storage),
  RunningDisk = config:search_for_application_value(scratch_disk, ?BH_RELATIVE_DIR("run"), storage),
  
  WorkingDir = lists:flatten([ScratchDisk, "/", App#app.name]),
  RunningDir = lists:flatten([RunningDisk, "/", App#app.name]),
  
  HostIp = bh_host:myip(),
  Id = {App#app.name, HostIp, Port},
  StartedAt = date_util:now_to_seconds(),
  
  OtherOpts = [
    {bee_image, ImagePath},
    {host_ip, HostIp},
    {port, misc_utils:to_list(Port)},
    {start_time, misc_utils:to_list(StartedAt)},
    {working_directory, WorkingDir},
    {run_dir, RunningDir}
  ],
  EnvOpts = apps:build_app_env(App, OtherOpts),
  lists:map(fun(Dir) -> file:make_dir(Dir) end, [ScratchDisk, WorkingDir, RunningDisk, RunningDir]),
  CmdOpts = lists:flatten([{cd, RunningDir}, EnvOpts]),
  
  % StartProplist = ?APP_TEMPLATE_SHELL_SCRIPT_PARSED(Template, Vars, DefaultProps),
  % AppRootPath = proplists:get_value(path, Proplist1),
  
  Bee  = #bee{
    id                      = Id,
    app_name                = App#app.name,
    host                    = HostIp,
    host_node               = node(self()),
    storage_node            = StorageNode,
    % path                    = AppRootPath,
    port                    = Port,
    status                  = pending,
    commit_hash             = Sha,
    start_time              = StartedAt
  },
  
  io:format("------ App handler using babysitter spawn_new: ~p~n", [CmdOpts]),
  case babysitter:run(App#app.template, start, CmdOpts) of
    {ok, Pid, OsPid} ->
      NewBee = Bee#bee{pid = Pid, os_pid = OsPid},
      ets:insert(?TAB_ID_TO_BEE, {Id, NewBee}),
      ets:insert(?TAB_NAME_TO_BEE, {App#app.name, NewBee}),
      
      AppLauncher ! {started_bee, NewBee},
      NewBee;
    Else ->
      AppLauncher ! {error, Else}
  end.

% Find and transfer the bee
find_and_transfer_bee(App, Sha) ->
  Nodes = lists:map(fun(N) -> node(N) end, node_manager:get_servers(storage)),
  Path = apps:build_on_disk_app_name(App),
  
  ScratchDisk = config:search_for_application_value(scratch_disk, ?BH_RELATIVE_DIR("storage"), storage),
  
  LocalPath = filename:join([filename:absname(ScratchDisk), lists:append([Path, "/", App#app.name, ".bee"])]),
  case find_bee_on_storage_nodes(App, Sha, Nodes) of
    {ok, Node, RemotePath} ->
      ?LOG(info, "find_bee_on_storage_nodes found on ~p at ~p to ~p", [Node, LocalPath, RemotePath]),
      slugger:get(Node, RemotePath, LocalPath),
      {ok, Node, LocalPath};
    E -> 
      ?LOG(info, "find_bee_on_storage_nodes returned ~p instead of something useful", [E]),
      E
  end.

% Look on the node and see if it has the 
find_bee_on_storage_nodes(App, _Sha, []) -> 
  % ?NOTIFY({app, app_not_squashed, Name}),
  ?NOTIFY({app, updated, App}),
  {error, not_found};
find_bee_on_storage_nodes(App, Sha, [Node|Rest]) ->
  case rpc:call(Node, ?STORAGE_SRV, has_squashed_repos, [App, Sha]) of
    false -> find_bee_on_storage_nodes(App, Sha, Rest);
    Path -> {ok, Node, Path}
  end.

% kill the instance of the application  
internal_stop_instance(#bee{id = Id, pid = PidPort, port = Port, host = Host} = _CalledBee, App, From) when is_record(App, app) ->  
  #bee{commit_hash = Sha} = Bee = bees:find_by_id(Id),
  ?LOG(debug, "internal_stop_instance: ~p and ~p", [Sha, App#app.name]),
  
  % Send a SIGHUP
  babysitter:stop_process(PidPort),
  
  case ets:lookup(?TAB_ID_TO_BEE, {App#app.name, Host, Port}) of
    [{Key, _B}] ->
      ets:delete(?TAB_NAME_TO_BEE, App#app.name),
      ets:delete(?TAB_ID_TO_BEE, Key);
    _ -> true
  end,
  From ! {bee_terminated, Bee}.
  
handle_pid_exit(_Pid, _Reason, State) ->
  State.