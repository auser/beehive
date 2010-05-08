%%%-------------------------------------------------------------------
%%% File    : bh_storage_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec  3 10:38:18 PST 2009
%%%-------------------------------------------------------------------

-module (bh_storage_srv).
-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_cluster).

%% API
-export([
  start_link/0,
  can_pull_new_app/0,
  pull_repos/2,
  build_bee/2,
  locate_git_repo/1,
  lookup_squashed_repos/2,
  has_squashed_repos/2,
  seed_nodes/1
]).

%% gen_cluster callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_leave/4]).


-record(state, {
  scratch_disk,
  squashed_disk
}).

-define(SERVER, ?MODULE).
-define (TAB_NAME_TO_PATH, 'name_to_path_table').

%%====================================================================
%% API
%%====================================================================
seed_nodes(_State) -> global:whereis_name(node_manager).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
pull_repos(AppName, Caller) ->
  gen_cluster:cast(?SERVER, {pull_repos, AppName, Caller}).

build_bee(AppName, Caller) ->
  gen_cluster:cast(?SERVER, {build_bee, AppName, Caller}).

locate_git_repo(Name) ->
  gen_cluster:call(?SERVER, {locate_git_repo, Name}).

lookup_squashed_repos(App, Sha) ->
  handle_lookup_squashed_repos(App, Sha).

has_squashed_repos(App, Sha) ->
  case handle_lookup_squashed_repos(App, Sha) of
    [] -> false;
    E -> E
  end.

can_pull_new_app() ->
  gen_cluster:call(?SERVER, {can_pull_new_app}).

start_link() ->
  gen_cluster:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  Opts = [named_table, set],
  ets:new(?TAB_NAME_TO_PATH, Opts),
  
  {ok, #state{
    scratch_disk = config:search_for_application_value(scratch_disk, ?BH_RELATIVE_DIR("tmp"), storage),
    squashed_disk = config:search_for_application_value(squashed_storage, ?BH_RELATIVE_DIR("squashed"), storage)
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
handle_call({can_pull_new_app}, _From, State) ->
  {reply, true, State};
handle_call({locate_git_repo, App}, _From, State) ->
  Reply = handle_repos_lookup(App),
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
handle_cast({pull_repos, App, Caller}, State) ->
  case handle_repos_lookup(App) of
    {ok, _ReposUrl} -> 
      % TempName = lists:append([handle_find_application_location(App, State), "/home/app"]),
      % {Proplists, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("pull-git-repos", [
      %   {"[[GIT_REPOS]]", ReposUrl},
      %   {"[[DESTINATION]]", TempName}
      % ]),
      % io:format("Proplists: ~p~n", [Proplists]),
      % Reply = case proplists:is_defined(sha, Proplists) of
      %   true -> {pulled, Proplists};
      %   false -> {error, "Could not pull git repos"}
      % end,
      Reply = {pulled, []},
      Caller ! Reply,
      {noreply, State};
    _Else ->
      Reply = {error, "Not git url specified"},
      Caller ! Reply,
      {noreply, State}
  end;

handle_cast({build_bee, App, Caller}, #state{scratch_disk = ScratchDisk, squashed_disk = SquashedDisk} = State) ->
  {ok, ReposUrl} = handle_repos_lookup(App),
  % OutFile = lists:append([handle_find_application_location(App, State), ".img"]),
  % 
  % io:format("creating bee: ~p~n", [{ReposUrl, ScratchDisk, SquashedDisk, App#app.name, OutFile}]),
  % {Proplists, Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("create-bee", [
  %   {"[[GIT_REPOS]]", ReposUrl},
  %   {"[[WORKING_DIRECTORY]]", ScratchDisk},
  %   {"[[SQUASHED_DIRECTORY]]", SquashedDisk},
  %   {"[[APP_NAME]]", App#app.name},
  %   {"[[OUTFILE]]", OutFile}
  % ]),
  WorkingDir = lists:flatten([ScratchDisk, "/", App#app.name]),
  SquashedDir = lists:flatten([SquashedDisk, "/", App#app.name]),
  
  OtherOpts = [
    {working_directory, WorkingDir},
    {squashed_directory, SquashedDir},
    {repos, ReposUrl},
    {path, "/usr/bin:/usr/local/bin:/bin"}
  ],
  CmdOpts = apps:build_app_env(App, OtherOpts),
  
  lists:map(fun(Dir) -> file:make_dir(Dir) end, [WorkingDir, SquashedDir]),
  
  X = babysitter:run(App#app.template, bundle, CmdOpts),
  erlang:display({babysitter, X}),
  Caller ! {unimplemented, come_back_soon},
  {noreply, State};
  % Status = 1,
  % case Status of
  %   0 ->
  %     io:format("Proplists: ~p~n", [Proplists]),
  %     Reply = case proplists:is_defined(bee_size, Proplists) of
  %       true -> 
  %         Path = proplists:get_value(outdir, Proplists),
  %         ets:insert(?TAB_NAME_TO_PATH, {App, Path}),
  %         {bee_built, Proplists};
  %       false -> 
  %         {error, "Could not create bee"}
  %     end,
  %     Caller ! Reply,
  %     {noreply, State};
  %   1 ->
  %     Reply = {error, could_not_pull_bee},
  %     Caller ! Reply,
  %     {noreply, State};
  %   Else ->
  %     Reply = {error, Else},
  %     Caller ! Reply,
  %     {noreply, State}
  % end;

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_cluster when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_cluster terminates with Reason.
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
handle_repos_lookup(AppName) ->
  case config:search_for_application_value(git_store, offsite, storage) of
    offsite -> 
      {ok, handle_offsite_repos_lookup(AppName)};
    _ -> 
      io:format("Looking in local repos not yet supported~n"),
      {error, not_found}
  end.

handle_offsite_repos_lookup([]) -> false;
handle_offsite_repos_lookup(App) when is_record(App, app) ->
  App#app.url;
handle_offsite_repos_lookup(AppName) ->
  case apps:find_by_name(AppName) of
    App when is_record(App, app) -> 
      handle_offsite_repos_lookup(App);
    _ -> false
  end.

handle_lookup_squashed_repos(App, Sha) when is_record(App, app) ->
  handle_lookup_squashed_repos(App#app.name, Sha);
handle_lookup_squashed_repos(Name, Sha) ->
  case ets:lookup(?TAB_NAME_TO_PATH, lists:append([Name, Sha])) of
    [{_Key, Path}] -> Path;
    _ -> 
      SquashedDir = config:search_for_application_value(squashed_storage, ?BH_RELATIVE_DIR("squashed"), storage),
      {ok, Folders} = file:list_dir(SquashedDir),
      case lists:member(Name, Folders) of
        true ->
          Dir = filename:join([SquashedDir, Name]),
          FullFilePath = filename:join([Dir, lists:append([Name, ".", Sha, ".img"])]),
          case filelib:is_file(FullFilePath) of
            true -> FullFilePath;
            false -> false
          end;
        false ->
          false
      end
  end.  

handle_find_application_location(App, #state{scratch_disk = ScratchDisk} = _State) ->
  AppName = apps:build_on_disk_app_name(App),
  filename:join([ScratchDisk, AppName]).
