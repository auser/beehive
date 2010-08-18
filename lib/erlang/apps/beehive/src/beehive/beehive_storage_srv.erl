%%%-------------------------------------------------------------------
%%% File    : bh_storage_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec  3 10:38:18 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_storage_srv).
-include ("beehive.hrl").
-include ("common.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(gen_cluster).

%% API
-export([
  start_link/0,
  fetch_or_build_bee/2,
  build_bee/1, build_bee/2,
  seed_nodes/1
]).

%% gen_cluster callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% gen_cluster callback
-export([handle_join/2, handle_leave/3, handle_vote/2]).


-record(state, {
  squashed_disk
}).

-define(SERVER, ?MODULE).
-define (TAB_NAME_TO_PATH, 'name_to_path_table').

%%====================================================================
%% API
%%====================================================================
seed_nodes(_State) -> [node(seed_pid())].

seed_pid() -> hd(seed_pids([])).
seed_pids(_State) ->
  case global:whereis_name(?MODULE) of
    undefined -> [self()]; % We are the master
    _ ->
      {ok, Plist} = gen_cluster:plist(?MODULE),
      Plist
  end.
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
build_bee(App) when is_record(App, app) ->         build_bee(App, undefined).
build_bee(Name) ->
  case apps:find_by_name(Name) of
    App when is_record(App, app) -> build_bee(App, undefined);
    _ -> {error, app_not_found}
  end.
build_bee(App, Caller) when is_record(App, app) -> gen_cluster:call(?SERVER, {build_bee, App, Caller}, infinity);
build_bee(Name, Caller) ->
  case apps:find_by_name(Name) of
    App when is_record(App, app) -> build_bee(App, Caller);
    _ -> {error, app_not_found}
  end.

fetch_or_build_bee(App, Caller) ->
  gen_cluster:call(?SERVER, {fetch_or_build_bee, App, Caller}, infinity).

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
  SquashedDir = config:search_for_application_value(squashed_storage, ?BEEHIVE_DIR("squashed")),
  
  bh_file_utils:ensure_dir_exists([SquashedDir]),
  
  {ok, #state{
    squashed_disk = SquashedDir
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
handle_call({build_bee, App, Caller}, _From, State) ->
  Resp = case internal_build_bee(App, Caller, State) of
    {error, {ExitCode, Reasons}} -> 
      Error = #app_error{
        stage = bundle, % erm?
        stdout = lists:reverse(Reasons),
        exit_status = ExitCode,
        timestamp = date_util:now_to_seconds()
      },
      {ok, NewApp} = app_manager:request_to_save_app(App#app{latest_error = Error}),
      {error, NewApp};
    Props when is_list(Props) ->
      {updated, NewApp} = apps:update(App#app{latest_error = undefined}, Props),
      Bee = bees:new(Props),
      {ok, NewApp, Bee}
  end,
  {reply, Resp, State};
  
handle_call({fetch_or_build_bee, App, Caller}, _From, State) ->
  Resp = case fetch_bee(App, Caller, State) of
    {error, _} -> internal_build_bee(App, Caller, State);
    T -> T
  end,
  {reply, Resp, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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

% beehive_bee_object:send_bee_object(node(Caller), Name)
% Info = beehive_bee_object:info(Name),
% erlang:display({fetch_bee, Info}),
% ?NOTIFY({bee, bee_built, Info}),
% Caller ! {bee, bee_built, Info},
% Info.
handle_info(Info, State) ->
  erlang:display({got, Info}),
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


% HANDLE VOTING
handle_vote(_Msg, State) ->
  {reply, 0, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
fetch_bee(#app{name = Name} = App, Caller, _State) ->
  case lists:member(Name, beehive_bee_object:ls()) of
    true -> 
      % We need to check to make sure this is the latest bee...
      beehive_bee_object:send_bee_object(node(Caller), Name, Caller);
    false -> 
      beehive_bee_object:bundle(apps:to_proplist(App), Caller)
  end.

%%-------------------------------------------------------------------
%% @spec (App::app(), State) ->    {ok, Value}
%% @doc This will call the bundle task on the application template
%%  and bundle the application into a known file: NAME.bee
%%      
%% @end
%%-------------------------------------------------------------------
internal_build_bee(App, Caller, _State) ->
  case handle_repos_lookup(App) of
    {ok, ReposUrl} ->
      beehive_bee_object:bundle(apps:to_proplist(App#app{url = ReposUrl}), Caller);
    {error, _} = T -> T
    %   case babysitter_integration:command(bundle, App#app{url = ReposUrl}, unusued, Proplist) of
    %     {ok, _OsPid, 0} ->
    %       case fetch_bee(App, State) of
    %         {bee_built, _Resp} = T -> T;
    %         E -> E
    %       end;
    %     {error, Stage, _OsPid, ExitCode, Stdout, Stderr} ->
    %       % stage,        % stage at which the app failed
    %       % stderr,       % string with the stderr
    %       % stdout,       % string with the stdout
    %       % exit_status,  % exit status code
    %       % timestamp     % time when the exit happened
    %       Error = #app_error{
    %         stage = Stage,
    %         stderr = Stderr,
    %         stdout = Stdout,
    %         exit_status = ExitCode,
    %         timestamp = date_util:now_to_seconds()
    %       },
    %       % {ok, NewApp} = app_manager:request_to_save_app(App#app{latest_error = Error}),
    %       {error, {babysitter, App#app{latest_error = Error}}};
    %     Else ->
    %       erlang:display({got_something_else,babysitter_run, Else}),
    %       {error, Else}
    %   end;
    % {error, _} = T -> T
  end.
  
handle_repos_lookup(App) ->
  case config:search_for_application_value(git_store, offsite) of
    offsite -> 
      {ok, handle_offsite_repos_lookup(App)};
    _ -> 
      io:format("Looking in local repos not yet supported~n"),
      {error, repos_not_found}
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
