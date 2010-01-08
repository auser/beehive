%%%-------------------------------------------------------------------
%%% File    : babysitter.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec 24 15:10:38 PST 2009
%%%-------------------------------------------------------------------

-module (babysitter).
-behaviour(gen_server).

%% API
-export ([
  spawn_new/2,
  stop_process/1,
  isolate_command/0
]).

-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% only for tests
-export ([
  build_exec_opts/2,
  build_isolate_command/1
]).

-record(state, {}).
-define(SERVER, ?MODULE).
-define (PID_TO_CALLER, 'pid_to_caller_table').
%%====================================================================
%% API
%%====================================================================
spawn_new(Options, From) -> 
  gen_server:call(?SERVER, {spawn_new, Options, From}).

stop_process(Arg) ->
  handle_stop_process(Arg).

isolate_command() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  filename:join([Dir, "priv", "bin", "isolate"]).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, stop).
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
  ets:new(?PID_TO_CALLER, Opts),
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
handle_call({spawn_new, Options, From}, _From, State) ->
  Pid = handle_spawn_new(Options),
  ets:insert(?PID_TO_CALLER, {Pid, From}),
  {reply, Pid, State};
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
handle_info({'EXIT', Pid, _Status} = Tuple, State) ->
  io:format("Os process: ~p died~n", [Pid]),
  case ets:lookup(?PID_TO_CALLER, Pid) of
    [{Pid, Caller}] -> 
      ets:delete(?PID_TO_CALLER, Pid),
      Caller ! Tuple;
    _ -> ok
  end,
  {noreply, State};
handle_info(Info, State) ->
  io:format("Info in babysitter: ~p~n", [Info]),
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
handle_spawn_new(Opts) ->
  RealCommand = build_isolate_command(Opts),
  ExecOpts = build_exec_opts(Opts, []),
  io:format("RealCommand: ~p and opts: ~p~n", [RealCommand, ExecOpts]),
  exec:run_link(RealCommand, ExecOpts).

handle_stop_process(Pid) when is_pid(Pid) ->
  babysitter_process:stop(Pid).

build_isolate_command(Opts) ->
  Vars    = fetch_value(vars, Opts),
  DefinedCommand = fetch_value(start_command, Opts),
  Command = string_utils:template_command_string(DefinedCommand, Vars),
  
  SkelOrDirs = case fetch_value(skel, Opts) of
    undefined ->
      AlwaysIncludedDir = " -D /bin -D /lib",
      case fetch_value(dirs, Opts) of
        [] -> AlwaysIncludedDir;
        Dirs -> lists:flatten([AlwaysIncludedDir, " -D ", string:join(Dirs, " -D ")])
      end;
      Skel -> lists:flatten([" -b ", Skel])
  end,
  ConfineDirectory = build_cli_option("-C", confine_dir, Opts),
  ProcessCount = build_cli_option("-p", num_processes, Opts),
  FilesCount = build_cli_option("-f", files_count, Opts),
  Mount = build_cli_option("-i", image, Opts),
  Env = build_cli_option("-e", env, Opts),
  
  lists:flatten(["exec ", 
    babysitter:isolate_command(), 
    ConfineDirectory,
    SkelOrDirs,
    ProcessCount,
    FilesCount,
    Mount,
    Env,
    " ",
    Command
  ]).

% Build the command-line option
build_cli_option(Switch, Param, Opts) -> 
  case fetch_value(Param, Opts) of
    [] -> [];
    undefined -> [];
    E -> lists:flatten([" ", Switch, " ", E])
  end.

% Accept only know execution options
build_exec_opts([], Acc) -> Acc;
build_exec_opts([{cd, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{env, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{nice, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{stdout, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{stderr, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{kill, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{user, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([_Else|Rest], Acc) -> build_exec_opts(Rest, Acc).

% Fetch values and defaults
fetch_value(vars, Opts) -> opt_or_default(vars, [], Opts);
fetch_value(env, Opts) -> opt_or_default(env, [], Opts);
fetch_value(num_processes, Opts) -> opt_or_default(num_processes, "5", Opts);
fetch_value(files_count, Opts) -> opt_or_default(files_count, "5", Opts);
fetch_value(confine_dir, Opts) -> opt_or_default(confine_dir, "/var/confine", Opts);
fetch_value(skel, Opts) -> opt_or_default(skel, undefined, Opts);
fetch_value(image, Opts) -> opt_or_default(image, undefined, Opts);
fetch_value(dirs, Opts) -> opt_or_default(dirs, [], Opts);
fetch_value(start_command, Opts) -> opt_or_default(start_command, "thin -- -R config.ru start", Opts).

opt_or_default(Param, Default, Opts) ->
  case proplists:get_value(Param, Opts) of
    undefined -> Default;
    V -> V
  end.
