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
  stop_instance/2, cleanup_instance/2, unmount_instance/2,
  stop_app/2,
  can_deploy_new_app/0,
  has_app_named/1,
  seed_nodes/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
% gen_cluster callback
-export([handle_join/2, handle_leave/3]).

-export ([handle_vote/2]).

-record(state, {
  run_directory,
  scratch_dir,
  max_bees             % maximum number of bees on this host
}).
-define(SERVER, ?MODULE).

-define (STORAGE_SRV, beehive_storage_srv).

-define (TAB_ID_TO_BEE, 'id_to_bee_table').
-define (TAB_NAME_TO_BEE, 'name_to_bee_table').

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
seed_nodes(_State) -> [node(seed_pid())].
seed_pid() -> hd(seed_pids([])).
seed_pids(_State) ->
  case global:whereis_name(?MODULE) of
    undefined -> [self()]; % We are the master
    _ ->
      {ok, Plist} = gen_cluster:plist(?MODULE),
      Plist
  end.

start_link() ->
  gen_cluster:start_link({local, ?SERVER}, ?SERVER, [], []).

stop() ->
  gen_cluster:call(?SERVER, {stop}).
  
can_deploy_new_app() ->
  gen_cluster:call(?SERVER, {can_deploy_new_app}).
  
start_new_instance(App, Sha, AppLauncher, From) ->
  gen_cluster:call(?SERVER, {start_new_instance, App, Sha, AppLauncher, From}, infinity).

stop_instance(Bee, From) when is_record(Bee, bee) -> gen_cluster:cast(?SERVER, {stop_instance, Bee, From});
stop_instance(Else, _) -> {error, Else}.
unmount_instance(Bee, From) when is_record(Bee, bee) -> gen_cluster:cast(?SERVER, {unmount_instance, Bee, From});
unmount_instance(Else, _) -> {error, Else}.
cleanup_instance(Bee, From) when is_record(Bee, bee) -> gen_cluster:cast(?SERVER, {cleanup_instance, Bee, From});
cleanup_instance(Else, _) -> {error, Else}.

has_app_named(Name) ->
  gen_cluster:call(?SERVER, {has_app_named, Name}).
  
stop_app(App, From) ->
  gen_cluster:cast(?SERVER, {stop_app, App, From}).

handle_vote(_Msg, State) ->
  {reply, 0, State}.
  
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
  spawn(fun() -> ets:new(?TAB_ID_TO_BEE, Opts) end),
  spawn(fun() -> ets:new(?TAB_NAME_TO_BEE, Opts) end),
  
  MaxBackends     = ?MAX_BACKENDS_PER_HOST,
  
  ScratchDisk = config:search_for_application_value(scratch_dir, ?BEEHIVE_DIR("tmp")),
  RunDir = config:search_for_application_value(squashed_storage, ?BEEHIVE_DIR("run")),
  
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
handle_call({start_new_instance, App, Sha, AppLauncher, From}, _From, State) ->
  Port = bh_host:unused_port(),
  % Then start it :)
  ?LOG(debug, "internal_start_new_instance: ~p, ~p, ~p, ~p, ~p~n", [App, Sha, Port, AppLauncher, From]),
  Reply = internal_start_new_instance(App, Sha, Port, AppLauncher, From, State),
  {reply, Reply, State};

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
handle_cast({stop_instance, Bee, From}, State) ->
  spawn(fun() -> From ! internal_stop_instance(Bee, State) end),
  {noreply, State};
handle_cast({unmount_instance, Bee, From}, State) ->
  spawn(fun() -> From ! internal_unmount_instance(Bee, State) end),
  {noreply, State};
handle_cast({cleanup_instance, Bee, From}, State) ->
  spawn(fun() -> From ! internal_cleanup_instance(Bee, State) end),
  {noreply, State};
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
% Start new instance
internal_start_new_instance(App, Sha, Port, AppLauncher, From, State) ->
  case find_and_transfer_bee(App, Sha) of
    {ok, Node, LocalPath} ->
      Proplists = [{revision, Sha}, {port, Port}, {bee_image, LocalPath}, {storage_node, Node}],
      
      case mount_application(App, Proplists, State) of
        {ok, _OtherPid, _Status} ->
          initialize_application(App, Proplists, AppLauncher, From);
        Error -> Error
      end;
    E -> 
      io:format("[~p] Error: ~p~n", [?MODULE, E]),
      E
  end.

% Run the mount action on the app
mount_application(App, OtherPropLists, #state{scratch_dir = ScratchDisk, run_directory = RunDir} = _State) -> 
  Proplist = lists:flatten([
    {scratch_dir, ScratchDisk},
    {run_directory, RunDir},
    OtherPropLists
  ]),
  babysitter_integration:command(mount, App, unused, Proplist).

% Initialize the node
initialize_application(App, PropLists, AppLauncher, _From) ->
  case babysitter_integration:command(start, App, unused, PropLists) of
    {ok, Pid, OsPid, Bee} ->
      NewBee = Bee#bee{pid = Pid, os_pid = OsPid},
      (catch ets:insert(?TAB_ID_TO_BEE, {Bee#bee.id, NewBee})),
      (catch ets:insert(?TAB_NAME_TO_BEE, {App#app.name, NewBee})),
      AppLauncher ! {started_bee, NewBee},
      NewBee;
    Else ->
      erlang:display({error, starting_app, Else}),
      AppLauncher ! {error, Else}
  end.

% Find and transfer the bee
find_and_transfer_bee(App, Sha) ->
  ScratchDisk = config:search_for_application_value(scratch_dir, ?BEEHIVE_DIR("storage")),
  LocalPath = filename:join([filename:absname(ScratchDisk), lists:append([App#app.name, ".bee"])]),
  
  case gen_cluster:run(beehive_storage_srv, {has_squashed_repos, App, Sha}) of
    {error, Reason} ->
      % No nodes wanted to play, so let's force one
      erlang:display({error, ballot_run, Reason});
    {ok, Pid1, Result} -> 
      case Result of
        {ok, Node, RemotePath} ->
          % We have a node with the repos
          fetch_bee_from_into(Node, RemotePath, LocalPath),
          {ok, Node, LocalPath};
        {error, not_found} ->
          % We have not found the repos, so instruct this backend to pull the repos
          % because we have to fulfill the request, regardless
          build_and_then_fetch_into(Pid1, App, Sha, LocalPath);
        Else ->
          erlang:display({error, Else})
      end
  end.
  
% Build the application and then fetch it
build_and_then_fetch_into(Pid, App, Sha, LocalPath) ->
  case gen_cluster:call(Pid, {build_bee, App}, infinity) of
    {bee_built, Props} ->
      % Since we know that the bundle has been built somewhere,
      % we can fetch it now
      CurrentSha = proplists:get_value(revision, Props),
      case gen_cluster:call(Pid, {has_squashed_repos, App#app{revision = CurrentSha}, Sha}) of
        {ok, Node, RemotePath} -> 
          fetch_bee_from_into(Node, RemotePath, LocalPath), 
          {ok, Node, LocalPath};
        {error, not_found} ->
          throw({fata, {cannot_find_bee}})
      end;
    E ->
      erlang:display({error, E}),
      error
  end.

fetch_bee_from_into(Pid, RemotePath, LocalPath) ->
  % Make sure the directory exists  
  bh_file_utils:ensure_dir_exists([filename:dirname(LocalPath)]),
  % Get the squashed file
  slugger:get(Pid, RemotePath, LocalPath).

% kill the instance of the application  
internal_stop_instance(Bee, _State) ->  
  case babysitter_integration:command(stop, unused, Bee, []) of
    {ok, _OsPid, _ExitStatus, App} ->
      case ets:lookup(?TAB_ID_TO_BEE, Bee#bee.id) of
        [{Key, _B}] ->
          catch ets:delete(?TAB_NAME_TO_BEE, App#app.name),
          catch ets:delete(?TAB_ID_TO_BEE, Key);
        _ -> true
      end,      
      {bee_terminated, Bee};
    {error, no_command} -> {bee_terminated, Bee};
    Else -> {error, Else}
  end.

internal_unmount_instance(Bee, _State) ->
  case babysitter_integration:command(unmount, unused, Bee, []) of
    {ok, _OsPid} -> {bee_unmounted, Bee};
    {error, no_command} -> {bee_unmounted, Bee};
    Else -> {error, Else}
  end.

internal_cleanup_instance(Bee, _State) ->
  case babysitter_integration:command(cleanup, unused, Bee, []) of
    {ok, _OsPid} -> {bee_unmounted, Bee};
    {error, no_command} -> {bee_unmounted, Bee};
    Else -> {error, Else}
  end.
  

handle_pid_exit(_Pid, _Reason, State) ->
  State.

% Babysitter commands