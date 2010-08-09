%%%-------------------------------------------------------------------
%%% File    : app_killer_fsm.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Nov 18 17:30:15 PST 2009
%%%-------------------------------------------------------------------

-module (app_killer_fsm).
-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_fsm).

%% API
-export([start_link/2]).

% methods
-export ([
  kill/1
]).
% states
-export ([
  preparing/2,
  killing/2,
  unmounting/2,
  cleaning_up/2
]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record (state, {
  node,
  bee,
  from
}).

%%====================================================================
%% API
%%====================================================================
kill(Pid) -> gen_fsm:send_event(Pid, {kill}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(Bee, From) ->
  gen_fsm:start_link(?MODULE, [Bee, From], []).

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
init([Bee, From]) ->
  RpcNode = case Bee#bee.host_node of
    undefined -> node();
    _ -> Bee#bee.host_node
  end,
  State = #state{from = From, bee = Bee, node = RpcNode},
  {ok, preparing, State}.

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
preparing({kill}, #state{bee = #bee{app_name = Name} = _Bee, node = Node} = State) ->
  erlang:display({node, Node}),
  % If there is no node, we'll assume it's on the localhost
  rpc:call(Node, beehive_bee_object, stop, [default, Name, self()]),
  {next_state, killing, State};

preparing(Other, State) ->
  {stop, {received_unknown_message, {preparing, Other}}, State}.

killing({stopped, _BeeO}, #state{bee = #bee{app_name = Name} = Bee, node = Node} = State) ->
  bees:save(Bee#bee{status = stopped}),
  rpc:call(Node, beehive_bee_object, unmount, [default, Name, self()]),
  {next_state, unmounting, State};

killing(Msg, State) ->
  {stop, {received_unknown_message, {unmounting, Msg}}, State}.

unmounting({unmounted, _BeeObject}, #state{bee = #bee{app_name = Name} = _Bee, node = Node} = State) ->
  rpc:call(Node, beehive_bee_object, cleanup, [Name, self()]),
  {next_state, cleaning_up, State};

unmounting({error, Msg}, State) ->
  erlang:display({error, unmounting, Msg}),
  {stop, {error, Msg}, State}.

cleaning_up({cleaned_up, _BeeObject}, #state{from = From, bee = Bee} = State) ->
  erlang:display({cleaned_up, Bee}),
  % App stopped normally
  From ! {bee_terminated, Bee#bee{status = down}},
  {stop, normal, State};
  
cleaning_up(Event, State) ->
  ?LOG(info, "Got uncaught event in cleaning_up state: ~p", [Event]),
  {next_state, cleaning_up, State}.
  
state_name(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
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
state_name(Event, _From, State) ->
  Reply = ok,
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
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
handle_event(Event, StateName, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, StateName]),
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
% Handle port messages first
handle_info({data, Msg}, StateName, State) ->
  erlang:display({got,data,StateName, Msg}),
  {next_state, StateName, State};
handle_info({port_closed, _Port}, StateName, State) -> {next_state, StateName, State};
handle_info(Info, StateName, State) ->
  apply(?MODULE, StateName, [Info, State]).
  % {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
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