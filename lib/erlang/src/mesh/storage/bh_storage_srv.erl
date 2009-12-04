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
-include ("git.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  can_pull_new_app/0,
  pull_repos/3,
  build_bee/3,
  locate_git_repo/1,
  lookup_squashed_repos/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  scratch_disk
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
pull_repos(App, Proplist, AppUpdatorPid) ->
  gen_server:call(?SERVER, {pull_repos, App, Proplist, AppUpdatorPid}).

build_bee(App, Opts, AppUpdatorPid) ->
  gen_server:call(?SERVER, {build_bee, App, Opts, AppUpdatorPid}).

locate_git_repo(Name) ->
  gen_server:call(?SERVER, {locate_git_repo, Name}).

lookup_squashed_repos(Name) ->
  gen_server:call(?SERVER, {lookup_squashed_repos, Name}).

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
handle_call({pull_repos, App, Proplist, AppUpdatorPid}, _From, #state{scratch_disk = ScratchDisk} = State) ->
  {ok, ReposUrl} = handle_repos_lookup(App),
  TempName = lists:append([proplists:get_value(temp_name, Proplist), "/home/app"]),
  Templated = string_utils:template_command_string(?SHELL_SCRIPT("pull-git-repos"),
        [
          {"[[GIT_REPOS]]", ReposUrl},
          {"[[DESTINATION]]", TempName}
        ]),
      
  Port = port_handler:start(Templated, ScratchDisk, self(), [use_stdio]),
  
  Reply = wait_for_port(Port, AppUpdatorPid, pulled, ?COULD_NOT_PULL_GIT_ERROR, []),
  {reply, Reply, State};

handle_call({build_bee, _App, Proplist, AppUpdatorPid}, _From, #state{scratch_disk = ScratchDisk} = State) ->
  TempName = proplists:get_value(temp_name, Proplist),
  OutFile = filename:join([ScratchDisk, lists:append([TempName, ".iso"])]),
  Templated = string_utils:template_command_string(?SHELL_SCRIPT("create_bee"),
        [
          {"[[WORKING_DIRECTORY]]", ScratchDisk},
          {"[[APP_NAME]]", proplists:get_value(temp_name, Proplist)},
          {"[[OUTFILE]]", OutFile}
        ]),
      
  Port = port_handler:start(Templated, ScratchDisk, self(), [use_stdio]),
  Reply = wait_for_port(Port, AppUpdatorPid, bee_built, ?COULD_NOT_CREATE_BEE_ERROR, []),
  {reply, Reply, State};

handle_call({locate_git_repo, App}, _From, State) ->
  Reply = handle_repos_lookup(App),
  {reply, Reply, State};
handle_call({lookup_squashed_repos, Name}, _From, State) ->
  Reply = handle_lookup_squashed_repos(Name),
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
handle_repos_lookup(App) when is_record(App, app) ->
  case config:search_for_application_value(git_store, offsite, storage) of
    offsite -> 
      {ok, handle_offsite_repos_lookup(App)};
    _ -> 
      io:format("Looking in local repos not yet supported~n"),
      {error, not_found}
  end.

handle_offsite_repos_lookup(App) ->
  App#app.url.

handle_lookup_squashed_repos(_Name) ->
  ok.
  
wait_for_port(Port, AppUpdatorPid, SuccessMsg, ErrCode, Acc) ->
  receive
    {data, Data} ->
      wait_for_port(Port, AppUpdatorPid, SuccessMsg, ErrCode, [Data|Acc]);
    {port_exited, _Code} ->
      io:format("Data: ~p~n", [Acc]),
      AppUpdatorPid ! {error, ErrCode};
    {port_closed, _Pid} ->
      AppUpdatorPid ! {SuccessMsg, lists:reverse(Acc)}
  end.