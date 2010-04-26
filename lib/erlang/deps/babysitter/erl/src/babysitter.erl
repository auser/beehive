%%% babysitter
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 2010 Ari Lerner <arilerner@mac.com>
%% @doc Babysitter monitors and watches OS processes
%%      Similar to how the supervisors work with the
%%      erlang processes
-module (babysitter).
-behaviour(gen_server).
-include ("babysitter.hrl").

%% API
-export ([
  run/3,
  running/1,
  status/1,
  list/0
]).
% PRIVATE
% These are exported for testing reasons only
-export ([
  bs_spawn_run/2, bs_run/2, kill_pid/1
]).

-export([start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% only for tests
-export ([
  build_port_command/1
]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
run(AppType, Action, Opts) ->
  % APP_PID_TABLE
  Config = case babysitter_config:get(AppType, Action) of
    {error, _Reason} -> babysitter_config:get(default, Action);
    {ok, ActionPropList} -> ActionPropList
  end,
  ConfigCommand = element(2, Config),
  % Certainly cannot run without a command
  Command = case ConfigCommand of
    undefined -> element(2, babysitter_config:get(default, Action));
    E -> E
  end,
  Options = convert_config_to_runable_proplist([{do_before, 1}, {do_after, 3}], Config, Opts),
  
  case Action of
    start -> bs_spawn_run(Command, Options);
    _E -> bs_run(Command, Options)
  end.

running(Pid) ->
  case ets:lookup(?PID_MONITOR_TABLE, Pid) of
    [{_Key, _X}|_Rest] -> true;
    _ -> false
  end.

%%-------------------------------------------------------------------
%% @spec (Command::String, Options::proplist()) -> {ok, ErlangPid, OsPid}
%% @doc 
%% @end
%% @{4:@private}
%%-------------------------------------------------------------------
bs_spawn_run(Command, Options) -> gen_server:call(?SERVER, {port, {run, Command, Options}}).
% Give a maximum of 100 seconds to preform an action
bs_run(Command, Options) -> gen_server:call(?SERVER, {port, {exec, Command, Options}}, 30000).
kill_pid(Pid) -> gen_server:call(?SERVER, {port, {kill, Pid}}).
status(Pid) -> gen_server:call(?SERVER, {port, {status, Pid}}).
list() -> gen_server:call(?SERVER, {port, {list}}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

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
init([Options]) ->
  process_flag(trap_exit, true),  
  Exe   = build_port_command(Options),
  Debug = proplists:get_value(verbose, Options, default(verbose)),
  Config = proplists:get_value(config_dir, Options, default(config_dir)),
  try
    debug(Debug, "exec: port program: ~s\n", [Exe]),
    Port = erlang:open_port({spawn, Exe}, [binary, exit_status, {packet, 2}, nouse_stdio, hide]),
    babysitter_config:read(Config),
    {ok, #state{port=Port, debug=Debug}}
  catch _:Reason ->
    {stop, io:format("Error starting port '~p': ~200p", [Exe, Reason])}
  end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({port, {run, Command, Options}}, From, #state{last_trans=_Last} = State) -> 
  handle_port_call({run, Command, build_exec_opts(Options, [])}, From, State);
handle_call({port, {exec, Command, Options}}, From, #state{last_trans=_Last} = State) -> 
  handle_port_call({exec, Command, build_exec_opts(Options, [])}, From, State);
handle_call({port, {kill, OsPid}}, From, #state{last_trans=_Last} = State) -> handle_port_call({kill, OsPid}, From, State);
handle_call({port, {status, OsPid}}, From, #state{last_trans=_Last} = State) -> handle_port_call({status, OsPid}, From, State);
handle_call({port, {list}}, From, #state{last_trans=_Last} = State) -> handle_port_call({list}, From, State);
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
handle_info({Port, {data, Bin}}, #state{port=Port, debug=Debug, trans = Trans} = State) ->
  Term = erlang:binary_to_term(Bin),
  case Term of
    {0, {exit_status, OsPid, Status}} ->
        debug(Debug, "Pid ~w exited with status: {~w,~w}\n", [OsPid, (Status band 16#FF00 bsr 8), Status band 127]),
        os_process:notify_ospid_owner(OsPid, Status),
        {noreply, State};
    {N, Reply} when N =/= 0 ->
      case get_transaction(Trans, N) of
        {true, {Pid,_} = From, Q, Link} ->
          NewReply = add_monitor(Reply, Link, Pid, Debug),
          gen_server:reply(From, NewReply);
        {false, Q} ->
          ok
      end,
      {noreply, State#state{trans=Q}};
    _Else ->
      {noreply, State}
  end;
handle_info({Port, {exit_status, 0}}, #state{port=Port} = State) ->
  {stop, normal, State};
handle_info({Port, {exit_status, Status}}, #state{port=Port} = State) ->
  {stop, {exit_status, Status}, State};
handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
  {stop, Reason, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  % OsPid's Pid owner died. Kill linked OsPid.
  os_process:process_owner_died(Pid, Reason, State),
  {noreply, State};
handle_info(Info, State) ->
  erlang:display("Other info: " ++ Info),
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
handle_port_call(T, From, #state{last_trans = LastTrans, trans = OldTransQ} = State) ->
  try
    TransId = next_trans(LastTrans),
    erlang:port_command(State#state.port, term_to_binary({TransId, T})),
    Link = case element(1, T) of
      run -> true;
      _ -> false
    end,
    {noreply, State#state{trans = queue:in({TransId, From, Link}, OldTransQ)}}
  catch _:{error, Why} ->
    {reply, {error, Why}, State}
  end.
  
%% Add a link for Pid to OsPid if requested.
add_monitor({ok, OsPid}, true, Pid, Debug) when is_integer(OsPid) ->
  % This is a reply to a run/run_link command. The port program indicates
  % of creating a new OsPid process.
  % Spawn a light-weight process responsible for monitoring this OsPid
  Self = self(),
  Process = spawn_link(fun() -> os_process:start(Pid, OsPid, Self, Debug) end),
  ets:insert(?PID_MONITOR_TABLE, [{OsPid, Process}, {Process, OsPid}]),
  {ok, Process, OsPid};
add_monitor(Reply, false, _Pid, _Debug) ->
  Reply.

get_transaction(Q, I) -> get_transaction(Q, I, Q).
get_transaction(Q, I, OldQ) ->
  case queue:out(Q) of
    {{value, {I, From, Link}}, Q2} ->
      {true, From, Q2, Link};
    {empty, _} ->
      {false, OldQ};
    {_E, Q2} ->
      get_transaction(Q2, I, OldQ)
    end.


%%-------------------------------------------------------------------------
%% @spec () -> Default::exec_options()
%% @doc Provide default value of a given option.
%% @end
%%-------------------------------------------------------------------------
default() -> 
    [{debug, false},  
     {verbose, false},  
     {config_dir, filename:join([filename:dirname(code:which(?MODULE)), "..", "..", "config", "apps"]) },
     {port_program, default(port_program)}].

default(port_program) -> 
  % Get architecture (e.g. i386-linux)
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  filename:join([Dir, "..", "priv", "bin", "babysitter"]);
default(Option) ->
  search_for_application_value(Option).

% Look for it at the environment level first
search_for_application_value(Param) ->
  case application:get_env(babysitter, Param) of
    undefined         -> search_for_application_value_from_environment(Param);
    {ok, undefined}   -> search_for_application_value_from_environment(Param);
    {ok, V}    -> V
  end.

search_for_application_value_from_environment(Param) ->
  EnvParam = string:to_upper(erlang:atom_to_list(Param)),
  case os:getenv(EnvParam) of
    false -> proplists:get_value(Param, default());
    E -> E
  end.


% Build the port process command
build_port_command(Opts) ->
  Args = build_port_command1(Opts, []),
  proplists:get_value(port_program, Opts, default(port_program)) ++ lists:flatten([" -n"|Args]).

% Fold down the option list and collect the options for the port program
build_port_command1([], Acc) -> Acc;
build_port_command1([{verbose, _V} = T | Rest], Acc) -> build_port_command1(Rest, [port_command_option(T) | Acc]);
build_port_command1([{debug, X} = T|Rest], Acc) when is_integer(X) -> build_port_command1(Rest, [port_command_option(T) | Acc]);
build_port_command1([_H|Rest], Acc) -> build_port_command1(Rest, Acc).

% Purely to clean this up
port_command_option({debug, X}) when is_integer(X) -> io:fwrite(" --debug ~w", [X]);
port_command_option({debug, _Else}) -> " --debug 4";
port_command_option(_) -> "".

% Accept only know execution options
% Throw the rest away
build_exec_opts([], Acc) -> Acc;
build_exec_opts([{cd, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{env, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{nice, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{do_before, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([{do_after, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
% Get these back in? 
% build_exec_opts([{stdout, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
% build_exec_opts([{stderr, _V}=T|Rest], Acc) -> build_exec_opts(Rest, [T|Acc]);
build_exec_opts([_Else|Rest], Acc) -> build_exec_opts(Rest, Acc).

% PRIVATE
debug(false, _, _) ->     ok;
debug(true, Fmt, Args) -> io:format(Fmt, Args).

% So that we can get a unique id for each communication
next_trans(I) when I < 268435455 -> I+1;
next_trans(_) -> 1.

%%-------------------------------------------------------------------
%% @spec (Config::proplist()) ->    {ok, Value}
%% @doc Combine the proplist into a proper option proplist
%%      
%% @end
%% @private
%%-------------------------------------------------------------------
convert_config_to_runable_proplist([], _, Acc) -> lists:reverse(Acc);
convert_config_to_runable_proplist([{Key, Pos}|T], Config, Acc) -> 
  case element(Pos, Config) of
    undefined -> convert_config_to_runable_proplist(T, Config, Acc);
    E -> convert_config_to_runable_proplist(T, Config, [{Key, E}|Acc])
  end.
