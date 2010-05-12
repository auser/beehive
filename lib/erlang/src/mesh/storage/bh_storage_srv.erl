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
-include_lib("kernel/include/file.hrl").

-behaviour(gen_cluster).

%% API
-export([
  start_link/0,
  can_pull_new_app/0,
  fetch_or_build_bee/2,
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
fetch_or_build_bee(AppName, Caller) ->
  gen_cluster:cast(?SERVER, {fetch_or_build_bee, AppName, Caller}).

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
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({fetch_or_build_bee, App, Caller}, State) ->
  Resp = case fetch_bee(App, State) of
    {error, _} -> build_bee(App, State);
    T -> T
  end,
  Caller ! Resp,
  {noreply, State};

handle_cast({build_bee, App, Caller}, State) ->
  Caller ! build_bee(App, State),
  {noreply, State};

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
fetch_bee(App, #state{squashed_disk = SquashedDisk} = _State) ->
  SquashedDir = lists:flatten([SquashedDisk, "/", App#app.name]),
  FinalLocation = lists:flatten([SquashedDir, "/", App#app.name, ".bee"]),
  case filelib:is_file(FinalLocation) of
    true -> 
      Resp = bees:meta_data(FinalLocation),
      {bee_built, Resp};
    false -> {error, not_found}
  end.

%%-------------------------------------------------------------------
%% @spec (App::app(), State) ->    {ok, Value}
%% @doc This will call the bundle task on the application template
%%  and bundle the application into a known file: NAME.bee
%%      
%% @end
%%-------------------------------------------------------------------
build_bee(App, #state{scratch_disk = ScratchDisk, squashed_disk = SquashedDisk} = _State) ->
  case handle_repos_lookup(App) of
    {ok, ReposUrl} ->
      WorkingDir = lists:flatten([ScratchDisk, "/", App#app.name]),
      SquashedDir = lists:flatten([SquashedDisk, "/", App#app.name]),
      FinalLocation = lists:flatten([SquashedDir, "/", App#app.name, ".bee"]),
      EnvFileLocation = lists:flatten([SquashedDir, "/.env"]),
      lists:map(fun(Dir) -> file:make_dir(Dir) end, [ScratchDisk, WorkingDir, SquashedDisk, SquashedDir]),
  
      OtherOpts = [
        {working_directory, WorkingDir},
        {squashed_directory, SquashedDir},
        {env_file, EnvFileLocation},
        {squashed_file, FinalLocation},
        {repos, ReposUrl}
      ],
      CmdOpts = apps:build_app_env(App, OtherOpts),
      
      case babysitter:run(App#app.template, bundle, CmdOpts) of
        {ok, OsPid} ->
          ets:insert(?TAB_NAME_TO_PATH, {App, FinalLocation}),
          Resp1 = bees:meta_data(FinalLocation),
          Resp = lists:flatten([{os_pid, OsPid}|Resp1]),
          {bee_built, Resp};
        Else ->
          {error, {babysitter, Else}}
      end;
    {error, not_found} -> {error, not_found}
  end.
  
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
          FullFilePath = lists:flatten([Dir, "/", Name, ".bee"]),
          erlang:display({full_filepath, FullFilePath}),
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
