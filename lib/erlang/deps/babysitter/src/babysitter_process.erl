%%%-------------------------------------------------------------------
%%% File    : babysitter_process.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec 24 15:13:02 PST 2009
%%%-------------------------------------------------------------------

-module (babysitter_process).
-behaviour(gen_fsm).

%% API
-export([
  start_link/1,
  go/2,
  stop/1,
  get_state/1 % for debugging
]).

% states
-export ([
  initialize/2,
  running/2,
  stopping/2
]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record (state, {
  caller,
  start_command,
  stop_command,
  port_process,
  port_pid,
  port_options = [],
  vars = [],
  callbacks = [],
  captured_output = []
}).
%%====================================================================
%% API
%%====================================================================
go(Pid, From) ->
  gen_fsm:send_event(Pid, {go, From}).

stop(Pid) ->
  gen_fsm:send_event(Pid, {stop}).
  
get_state(Pid) ->
  gen_fsm:sync_send_all_state_event(Pid, get_state).

%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(Opts) ->
  gen_fsm:start_link(?MODULE, [Opts], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([Opts]) ->
  NewState = parse_port_options(Opts, #state{}),
  {ok, initialize, NewState}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------
initialize({go, From}, #state{start_command = StartCmd, port_options = Opts, vars = Vars} = State) ->
  Command = case StartCmd of
    undefined -> "thin -R config.ru start";
    AStartCmd -> string_utils:template_command_string(AStartCmd, Vars)
  end,
  RealCommand = lists:flatten(io_lib:format("#!/bin/sh \n echo $$ && ~s", [Command])),
  Port = open_port({spawn, RealCommand}, [exit_status, stderr_to_stdout|Opts]),
  PortPid = receive
    {Port, {data, PidStr}} ->
      string:strip(PidStr, both, $\n)
    after 2000 -> exit({error, port_pid})
  end,
  {next_state, running, State#state{port_process = Port, caller = From, port_pid = PortPid}};
  
initialize(_Event, State) ->
  {next_state, running, State}.

% The port process is running the command
running({Port, {data, Data}}, #state{port_process = Port, callbacks = Callbacks} = State) ->
  run_callback(on_data, {data, Data}, Callbacks),
  {next_state, running, State};

running({Port, {exit_status, 0}}, #state{port_process = Port} = State) ->
  {stop, normal, State};

running({Port, {exit_status, Code}}, #state{port_process = Port} = State) ->
  {next_state, {error, Code}, State};
    
running({stop}, State) ->
  kill_os_process(State),
  {next_state, stopping, State};

running(Event, State) ->
  io:format("Got Event: ~p when in state: ~p~n", [Event, running]),
  {next_state, running, State}.
  
stopping(_Event, State) ->
  {stop, normal, State}.

state_name(_Event, State) ->
  {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(get_state, _From, StateName, State) ->
  {reply, StateName, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
  apply(?MODULE, StateName, [Info, State]).
  
%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{port_process = Port, caller = Caller} = State) ->
  kill_os_process(State), % really duplicate work, but safe
  (catch port_close(Port)),
  Caller ! {port_exited, Port, self()},
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse_port_options([], State) -> State;
parse_port_options([{start_command, Cmd}|Rest], State)  -> parse_port_options(Rest, State#state{start_command = Cmd});
parse_port_options([{stop_command, Cmd}|Rest], State)   -> parse_port_options(Rest, State#state{stop_command = Cmd});
parse_port_options([{callbacks,Callbacks}|Rest], State) -> parse_port_options(Rest, State#state{callbacks = Callbacks});
parse_port_options([{variables, Vars}|Rest], State)     -> parse_port_options(Rest, State#state{vars = Vars});
parse_port_options([{cd,Dir}|Rest], #state{port_options = Opts} = State) -> 
  parse_port_options(Rest, State#state{port_options = [{cd,Dir}|Opts]});
parse_port_options([{K,V}|Rest], #state{port_options = Opts} = State) -> 
  parse_port_options(Rest, State#state{port_options = [{K,V}|Opts]}).

run_callback(CallbackName, Val, Callbacks) ->
  case proplists:get_value(CallbackName, Callbacks) of
    undefined -> ok;
    Fun -> Fun(Val)
  end.

kill_os_process(#state{port_pid = PortPid, vars = Vars, stop_command = StopCmd} = _State) ->
  StopVars = [{"[[PID]]", PortPid}|Vars],
  Command = case StopCmd of
    undefined -> string_utils:template_command_string("kill -9 [[PID]]", StopVars);
    ACommd -> 
      string_utils:template_command_string(ACommd, StopVars)
  end,
  open_port({spawn, Command}, [stderr_to_stdout]),
  receive
    _E -> ok
    after 2000 -> 
      % If it doesn't return after 2 seconds, seriously make it try again
      open_port({spawn, Command}, [stderr_to_stdout]),
      ok
  end.
  