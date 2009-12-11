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
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  can_pull_new_app/0,
  pull_repos/2,
  build_bee/2,
  locate_git_repo/1,
  lookup_squashed_repos/1,
  has_squashed_repos/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  scratch_disk
}).

-define(SERVER, ?MODULE).
-define (TAB_NAME_TO_PATH, 'name_to_path_table').

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
pull_repos(AppName, Caller) ->
  gen_server:call(?SERVER, {pull_repos, AppName, Caller}).

build_bee(AppName, Caller) ->
  gen_server:call(?SERVER, {build_bee, AppName, Caller}).

locate_git_repo(Name) ->
  gen_server:call(?SERVER, {locate_git_repo, Name}).

lookup_squashed_repos(Name) ->
  handle_lookup_squashed_repos(Name).

has_squashed_repos(Name) ->
  case handle_lookup_squashed_repos(Name) of
    [] -> false;
    E -> E
  end.

% get_next_available_storage
can_pull_new_app() ->
  gen_server:call(?SERVER, {can_pull_new_app}).

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
  Opts = [named_table, set],
  ets:new(?TAB_NAME_TO_PATH, Opts),
  
  {ok, #state{
    scratch_disk = config:search_for_application_value(scratch_disk, "/tmp/squashed", storage)
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
  
% pull a repos url
handle_call({pull_repos, AppName, Caller}, _From, State) ->
  case handle_repos_lookup(AppName) of
    {ok, ReposUrl} -> 
      App = apps:find_by_name(AppName), % YES, I know this needs to be optminized
      TempName = lists:append([handle_find_application_location(App, State), "/home/app"]),
      {Proplists, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("pull-git-repos", [
        {"[[GIT_REPOS]]", ReposUrl},
        {"[[DESTINATION]]", TempName}
      ]),
      io:format("Proplists: ~p~n", [Proplists]),
      Reply = case proplists:is_defined(sha, Proplists) of
        true -> {pulled, Proplists};
        false -> {error, "Could not pull git repos"}
      end,
      Caller ! Reply,
      {reply, Reply, State};
    _Else ->
      Reply = {error, "Not git url specified"},
      Caller ! Reply,
      {reply, Reply, State}
  end;

handle_call({build_bee, AppName, Caller}, _From, #state{scratch_disk = ScratchDisk} = State) ->
  App = apps:find_by_name(AppName),
  {ok, ReposUrl} = handle_repos_lookup(AppName),
  OutFile = lists:append([handle_find_application_location(App, State), ".squashfs"]),
  
  {Proplists, _Status} = ?TEMPLATE_SHELL_SCRIPT_PARSED("create-bee", [
    {"[[GIT_REPOS]]", ReposUrl},
    {"[[WORKING_DIRECTORY]]", ScratchDisk},
    {"[[APP_NAME]]", apps:build_on_disk_app_name(App)},
    {"[[OUTFILE]]", OutFile}
  ]),
  Reply = case proplists:is_defined(bee_size, Proplists) of
    true -> 
      Path = proplists:get_value(outdir, Proplists),
      ets:insert(?TAB_NAME_TO_PATH, {AppName, Path}),
      {bee_built, Proplists};
    false -> 
      {error, "Could not create bee"}
  end,
  Caller ! Reply,
  {reply, Reply, State};

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
handle_repos_lookup(AppName) ->
  case config:search_for_application_value(git_store, offsite, storage) of
    offsite -> 
      {ok, handle_offsite_repos_lookup(AppName)};
    _ -> 
      io:format("Looking in local repos not yet supported~n"),
      {error, not_found}
  end.

handle_offsite_repos_lookup(App) when is_record(App, app) ->
  App#app.url;
handle_offsite_repos_lookup(AppName) ->
  handle_offsite_repos_lookup(apps:find_by_name(AppName)).

handle_lookup_squashed_repos(Name) ->
  case ets:lookup(?TAB_NAME_TO_PATH, Name) of
    [{_Key, Path}] -> Path;
    _ -> []
  end.  

handle_find_application_location(App, #state{scratch_disk = ScratchDisk} = _State) ->
  AppName = apps:build_on_disk_app_name(App),
  filename:join([ScratchDisk, AppName]).
