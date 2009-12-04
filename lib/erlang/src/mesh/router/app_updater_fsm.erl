%%%-------------------------------------------------------------------
%%% File    : app_updater_fsm.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec  3 11:38:51 PST 2009
%%%-------------------------------------------------------------------

-module (app_updater_fsm).

-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_fsm).

%% API
-export([
  start_link/1,
  go/2,
  get_state/1 % for debugging
]).

% states
-export ([
  pulling/2,
  squashing/2,
  starting/2,
  success/2
]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-define (APP_HANDLER, app_handler).
-define (STORAGE_SRV, bh_storage_srv).

-record (state, {
  app,
  temp_name,
  commit_hash,
  bee_size,
  dir_size,
  storage_node,
  host_node,
  id,
  port,
  bee
}).

%%====================================================================
%% API
%%====================================================================
go(Pid, From) ->
  gen_fsm:send_event(Pid, {go, From}).
  
get_state(Pid) ->
  Pid ! {get_state}.
  
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(App) ->
  gen_fsm:start_link(?MODULE, [App], []).

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
init([App]) ->
  TempName = lists:append([App#app.name, "-", misc_utils:to_list(erlang:phash2(make_ref()))]),
  {ok, pulling, #state{app = App, temp_name = TempName}}.

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
pulling({go, _From}, #state{app = App, temp_name = TempName} = State) ->
  Node = node_manager:get_next_available_storage(),
  Opts = [
    {temp_name, TempName}
  ],
  rpc:call(Node, ?STORAGE_SRV, pull_repos, [App, Opts, self()]),
  {next_state, squashing, State#state{storage_node = Node}};
  
pulling(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, pulling]),
  {next_state, launching, State}.

squashing({pulled, Info}, #state{app = App, storage_node = Node, temp_name = TempName} = State) ->
  Opts = [
    {temp_name, TempName}
  ],
  rpc:call(Node, ?STORAGE_SRV, build_bee, [App, Opts, self()]),
  [Sha|_Rest] = Info,
  {next_state, starting, State#state{commit_hash = Sha}};

squashing({error, Code}, State) ->
  {stop, Code, State};

squashing(Event, State) ->
  io:format("squashing: ~p~n", [Event]),
  {next_state, squashing, State}.

starting({bee_built, [Info]}, #state{app = App} = State) ->
  % Return is bee_size dir_size
  [BeeSize|Rest] = string:tokens(Info, " "),
  [DirSize1|_Rest1] = Rest,
  % Strip off the last newline... stupid bash
  DirSize = hd(string:tokens(DirSize1, "\n")),
  
  Node = node_manager:get_next_available_host(),
  
  % rpc:call(Node, ?STORAGE_SRV, build_bee, [App, Opts, self()]),
  {ok, P} = app_launcher_fsm:start_link(App, Node),
  app_launcher_fsm:launch(P, self()),
  
  NewState = State#state{bee_size = BeeSize, dir_size = DirSize, host_node = Node},
  {next_state, success, NewState};

starting(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, starting]),
  {next_state, launching, State}.

success(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, success]),
  {stop, normal, State}.
  
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
handle_info({get_state}, _StateName, State) ->
  {reply, State, State};
handle_info({stop}, _StateName, State) ->
  {stop, normal, State};
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
