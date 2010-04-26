-module (babysitter_port).
-behaviour(gen_server).

%% External exports
-export([
  start/1, start_link/1, run/2, run_link/2, 
  which_children/0, kill/2, stop/1, ospid/1, status/1
]).

%% Internal exports
-export([default/0, default/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
  port,
  last_trans  = 0,            % Last transaction number sent to port
  trans       = queue:new(),  % Queue of outstanding transactions sent to port
  limit_users = [],           % Restricted list of users allowed to run commands
  registry    = ets:new(exec_mon, [protected,named_table]), % Pids to notify when an OsPid exits
  debug       = false
}).

%%-------------------------------------------------------------------------
%% @spec (Options::options()) -> {ok, Pid::pid()} | {error, Reason}
%% @doc Supervised start an external program manager.
%% @end
%%-------------------------------------------------------------------------
start_link(Options) when is_list(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

%%-------------------------------------------------------------------------
%% @equiv start_link/1
%% @doc Start of an external program manager without supervision.
%% @end
%%-------------------------------------------------------------------------
start(Options) when is_list(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Options], []).

%%-------------------------------------------------------------------------
%% @spec (Exe::string(), Options::cmd_options()) -> Result
%%          Result  = {ok, Pid::pid(), OsPid::integer()} | {error, Reason}
%% @doc Start an external program.
%% @end
%%-------------------------------------------------------------------------
run(Exe, Options) when is_list(Exe), is_list(Options) ->
  gen_server:call(?MODULE, {port, {start, {run, Exe, Options}, nolink}}, 30000).

%%-------------------------------------------------------------------------
%% @equiv run/2
%% @doc Start an external program and link to the OsPid. If OsPid exits,
%%      the calling process will be killed or if it's trapping exits, 
%%      it'll get {'EXIT', OsPid, Status} message.  If the calling process
%%      dies the OsPid will be killed.
%% @end
%%-------------------------------------------------------------------------
run_link(Exe, Options) when is_list(Exe), is_list(Options) ->
  gen_server:call(?MODULE, {port, {start, {run, Exe, Options}, link}}).

%%-------------------------------------------------------------------------
%% @spec () -> [OsPid::integer()]
%% @doc Get a list of children managed by port program.
%% @end
%%-------------------------------------------------------------------------
which_children() ->
  gen_server:call(?MODULE, {port, {list}}).

%%-------------------------------------------------------------------------
%% @spec (Pid, Signal::integer()) -> ok | {error, Reason}
%%          Pid   = pid() | OsPid
%%          OsPid = integer()
%% @doc Send a `Signal' to a child `Pid' or `OsPid'.
%% @end
%%-------------------------------------------------------------------------
kill(Pid, Signal) when is_pid(Pid); is_integer(Pid) ->
    gen_server:call(?MODULE, {port, {kill, Pid, Signal}}).

%%-------------------------------------------------------------------------
%% @spec (Pid) -> ok | {error, Reason}
%%          Pid   = pid() | OsPid
%%          OsPid = integer()
%% @doc Terminate a managed `Pid' or `OsPid' process.
%% @end
%%-------------------------------------------------------------------------
stop(Pid) when is_pid(Pid); is_integer(Pid) ->
    gen_server:call(?MODULE, {port, {stop, Pid}}, 30000).

%%-------------------------------------------------------------------------
%% @spec (Pid::pid()) -> ok | {error, Reason}
%%          Pid   = pid() | OsPid
%%          OsPid = integer()
%% @doc Terminate a managed `Pid' or `OsPid' process.
%% @end
%%-------------------------------------------------------------------------
ospid(Pid) ->
    Ref = make_ref(),
    Pid ! {{self(), Ref}, ospid},
    receive
    {Ref, Reply} -> Reply;
    Other -> Other
    after 7000 ->
        {error, timeout}
    end.

%%-------------------------------------------------------------------------
%% @spec (Status::integer()) -> 
%%          {status, ExitStatus::integer()} | 
%%          {signal, Signal::integer(), Core::boolean()}
%% @doc Decode the program's exit_status.
%% @end
%%-------------------------------------------------------------------------
status(Status) when is_integer(Status) ->
    case {Status band 16#FF00 bsr 8, (Status band 128) =:= 128, Status band 127} of
    {Stat, _, 0}      -> {status, Stat};
    {_, Core, Signal} -> {signal, Signal, Core}
    end.

%%-------------------------------------------------------------------------
%% @spec () -> Default::exec_options()
%% @doc Provide default value of a given option.
%% @end
%%-------------------------------------------------------------------------
default() -> 
    [{debug, false},    % Debug mode of the port program. 
     {verbose, false},  % Verbose print of events on the Erlang side.
     {args, ""},        % Extra arguments that can be passed to port program
     {alarm, 12},
     {user, ""},        % Run port program as this user
     {limit_users, []}, % Restricted list of users allowed to run commands
     {portexe, default(portexe)}].

default(portexe) -> 
    % Get architecture (e.g. i386-linux)
    Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
    filename:join([Dir, "priv", "bin", "babysitter"]);
default(Option) ->
    proplists:get_value(Option, default()).

get_opt({Option, Value}) -> {Option, Value};
get_opt(debug)           -> {debug, true}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% @private
%%-----------------------------------------------------------------------
init([Options]) ->
  process_flag(trap_exit, true),
  Args = lists:foldl(
    fun({debug, true},       Acc) -> [" --debug 0" | Acc];
       ({alarm, I},          Acc) -> [" -alarm "++integer_to_list(I) | Acc];
       ({args, Arg},         Acc) -> [" "++Arg | Acc];
       ({user, User}, Acc) when User =/= "" -> [" -user "++User | Acc];
       (_,                   Acc) -> Acc
    end, [], [get_opt(O) || O <- Options]),
  Exe   = proplists:get_value(portexe,     Options, default(portexe)) ++ lists:flatten([" -n"|Args]),
  Users = proplists:get_value(limit_users, Options, default(limit_users)),
  Debug = proplists:get_value(verbose,     Options, default(verbose)),
  try
    debug(Debug, "exec: port program: ~s\n", [Exe]),
    Port = erlang:open_port({spawn, Exe}, [binary, exit_status, {packet, 2}, nouse_stdio, hide]),
    {ok, #state{port=Port, limit_users=Users, debug=Debug}}
  catch _:Reason ->
    {stop, io:format("Error starting port '~s': ~200p", [Exe, Reason])}
  end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%----------------------------------------------------------------------
handle_call({port, Instruction}, From, #state{last_trans=Last} = State) ->
  try is_port_command(Instruction, State) of
    {ok, Term, Link} ->
      Next = next_trans(Last),
      io:format("{~p, ~p}~n", [Next, Term]),
      erlang:port_command(State#state.port, term_to_binary({Next, Term})),
      {noreply, State#state{trans = queue:in({Next, From, Link}, State#state.trans)}}
    catch _:{error, Why} ->
      {reply, {error, Why}, State}
  end;

handle_call(Request, _From, _State) ->
    {stop, {not_implemented, Request}}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%----------------------------------------------------------------------
handle_info({Port, {data, Bin}}, #state{port=Port, debug=Debug} = State) ->
    Term = binary_to_term(Bin),
    %io:format("Msg from port: ~p\n", [Term]),
    case Term of
    {0, {exit_status, OsPid, Status}} ->
        debug(Debug, "Pid ~w exited with status: {~w,~w}\n", [OsPid, (Status band 16#FF00 bsr 8), Status band 127]),
        notify_ospid_owner(OsPid, Status),
        {noreply, State};
    {N, Reply} when N =/= 0 ->
        case get_transaction(State#state.trans, N) of
        {true, {Pid,_} = From, MonType, Q} ->
            NewReply = maybe_add_monitor(Reply, Pid, MonType, Debug),
            gen_server:reply(From, NewReply);
        {false, Q} ->
            ok
        end,
        {noreply, State#state{trans=Q}};
    {0, _Ignore} ->
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
    do_unlink_ospid(Pid, Reason, State),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:info_msg("~w - unhandled message: ~p\n", [?MODULE, Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% @private
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%% @private
%%----------------------------------------------------------------------
terminate(_Reason, #state{}) ->
  error_logger:warning_msg("~w - exec process terminated\n", [self()]),
  ok.

%%%---------------------------------------------------------------------
%%% Internal functions
%%%---------------------------------------------------------------------

%% Add a link for Pid to OsPid if requested.
maybe_add_monitor({ok, OsPid}, Pid, MonType, Debug) when is_integer(OsPid) ->
    % This is a reply to a run/run_link command. The port program indicates
    % of creating a new OsPid process.
    % Spawn a light-weight process responsible for monitoring this OsPid
    Self = self(),
    LWP  = spawn_link(fun() -> ospid_init(Pid, OsPid, MonType, Self, Debug) end),
    ets:insert(exec_mon, [{OsPid, LWP}, {LWP, OsPid}]),
    {ok, LWP, OsPid};
maybe_add_monitor(Reply, _Pid, _MonType, _Debug) ->
    Reply.

%%----------------------------------------------------------------------
%% @spec (Pid, OsPid::integer(), LinkType, Parent, Debug::boolean()) -> void()
%% @doc Every OsPid is associated with an Erlang process started with
%%      this function. The `Parent' is the ?MODULE port manager that
%%      spawned this process and linked to it. `Pid' is the process
%%      that ran an OS command associated with OsPid. If that process
%%      requested a link (LinkType = 'link') we'll link to it.
%% @end
%% @private
%%----------------------------------------------------------------------
ospid_init(Pid, OsPid, LinkType, Parent, Debug) ->
  process_flag(trap_exit, true),
  case LinkType of
    link -> link(Pid); % The caller pid that requested to run the OsPid command & link to it. 
    _    -> ok
  end,
  ospid_loop({Pid, OsPid, Parent, Debug}).

ospid_loop({Pid, OsPid, Parent, Debug} = State) ->
  receive
    {{From, Ref}, ospid} ->
      From ! {Ref, {ok, OsPid}},
      ospid_loop(State);
    {'DOWN', OsPid, {exit_status, Status}} ->
      debug(Debug, "~w ~w got down message (~w)\n", [self(), OsPid, status(Status)]),
      % OS process died
      exit({exit_status, Status});
    {'EXIT', Pid, Reason} ->
      % Pid died
      debug(Debug, "~w ~w got exit from linked ~w: ~p\n", [self(), OsPid, Pid, Reason]),
      exit({owner_died, Reason});
    {'EXIT', Parent, Reason} ->
      % Port program died
      debug(Debug, "~w ~w got exit from parent ~w: ~p\n", [self(), OsPid, Parent, Reason]),
      exit({port_closed, Reason});
    Other ->
      error_logger:warning_msg("~w - unknown msg: ~p\n", [self(), Other]),
      ospid_loop(State)
  end.
    
notify_ospid_owner(OsPid, Status) ->
  % See if there is a Pid owner of this OsPid. If so, sent the 'DOWN' message.
  case ets:lookup(exec_mon, OsPid) of
    [{_OsPid, Pid}] ->
      unlink(Pid),
      Pid ! {'DOWN', OsPid, {exit_status, Status}},
      ets:delete(exec_mon, {Pid, OsPid}),
      ets:delete(exec_mon, {OsPid, Pid});
    [] ->
      %error_logger:warning_msg("Owner ~w not found\n", [OsPid]),
      ok
  end.

debug(false, _, _) ->
  ok;
debug(true, Fmt, Args) ->        
  io:format(Fmt, Args).

%%----------------------------------------------------------------------
%% @spec (Pid::pid(), Action, State::#state{}) -> 
%%          {ok, LastTok::integer(), LeftLinks::integer()}
%% @doc Pid died or requested to unlink - remove linked Pid records and 
%% optionally kill all OsPids linked to the Pid.
%% @end
%%----------------------------------------------------------------------
do_unlink_ospid(Pid, _Reason, State) ->
  case ets:lookup(exec_mon, Pid) of
    [{_Pid, OsPid}] when is_integer(OsPid) ->
      debug(State#state.debug, "Pid ~p died. Killing linked OsPid ~w\n", [Pid, OsPid]),
      ets:delete(exec_mon, {Pid, OsPid}),
      ets:delete(exec_mon, {OsPid, Pid}),
      erlang:port_command(State#state.port, term_to_binary({0, {stop, OsPid}}));
    _ ->
      ok 
  end.

get_transaction(Q, I) -> 
  get_transaction(Q, I, Q).
get_transaction(Q, I, OldQ) ->
  case queue:out(Q) of
    {{value, {I, From, LinkType}}, Q2} ->
      {true, From, LinkType, Q2};
    {empty, _} ->
      {false, OldQ};
    {_, Q2} ->
      get_transaction(Q2, I, OldQ)
    end.
    
is_port_command({start, {run, _Cmd, Options} = T, Link}, State) ->
    check_cmd_options(Options, State),
    {ok, T, Link};
is_port_command({list} = T, _State) -> 
    {ok, T, undefined};
is_port_command({stop, OsPid}=T, _State) when is_integer(OsPid) -> 
    {ok, T, undefined};
is_port_command({stop, Pid}, _State) when is_pid(Pid) ->
    case ets:lookup(exec_mon, Pid) of
    [{Pid, OsPid}]  -> {ok, {stop, OsPid}, undefined};
    []              -> throw({error, no_process})
    end;
is_port_command({kill, OsPid, Sig}=T, _State) when is_integer(OsPid),is_integer(Sig) -> 
    {ok, T, undefined};
is_port_command({kill, Pid, Sig}, _State) when is_pid(Pid),is_integer(Sig) -> 
    case ets:lookup(exec_mon, Pid) of
    [{Pid, OsPid}]  -> {ok, {kill, OsPid, Sig}, undefined};
    []              -> throw({error, no_process})
    end.

check_cmd_options([{cd, Dir}|T], State) when is_list(Dir) ->
    check_cmd_options(T, State);
check_cmd_options([{env, Env}|T], State) when is_list(Env) ->
  check_cmd_options(T, State);
    % case lists:filter(fun(S) -> is_list(S) =:= false end, Env) of
    % [] -> check_cmd_options(T, State);
    % L  -> throw({error, {invalid_env_value, L}})
    % end;
check_cmd_options([{do_before, Cmd}|T], State) when is_list(Cmd) ->
  check_cmd_options(T, State);
check_cmd_options([{do_after, Cmd}|T], State) when is_list(Cmd) ->
  check_cmd_options(T, State);
check_cmd_options([{kill, Cmd}|T], State) when is_list(Cmd) ->
  check_cmd_options(T, State);
check_cmd_options([{nice, I}|T], State) when is_integer(I), I >= -20, I =< 20 ->
  check_cmd_options(T, State);
check_cmd_options([{Std, I}|T], State) when Std=:=stderr, I=/=Std; Std=:=stdout, I=/=Std ->
  if I=:=null; I=:=stderr; I=:=stdout; is_list(I); 
    is_tuple(I), tuple_size(I)=:=2, element(1,I)=:="append", is_list(element(2,I)) ->  
      check_cmd_options(T, State);
    true -> 
      throw({error, io:format("Invalid ~w option ~p", [Std, I])})
  end;
check_cmd_options([{user, U}|T], State) when is_list(U), U =/= "" ->
  case lists:member(U, State#state.limit_users) of
    true  -> check_cmd_options(T, State);
    false -> throw({error, io:format("User ~s is not allowed to run commands!", [U])})
  end;
check_cmd_options([Other|_], _State) -> throw({error, {invalid_option, Other}});
check_cmd_options([], _State)        -> ok.
    
next_trans(I) when I < 268435455 -> I+1;
next_trans(_) -> 1.