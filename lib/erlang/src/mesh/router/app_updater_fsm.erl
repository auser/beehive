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
  bee,
  from
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
%% Function: init(Args) -> {ok, BeeName, Bee} |
%%                         {ok, BeeName, Bee, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([App]) when is_record(App, app) ->
  init([App#app.name]);

init([AppName])  ->
  {ok, pulling, #state{bee = #bee{app_name = AppName}}}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, Bee) -> {next_state, NextBeeName, NextBee}|
%%                             {next_state, NextBeeName,
%%                                NextBee, Timeout} |
%%                             {stop, Reason, NewBee}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name BeeName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------
pulling({go, From}, #state{bee = #bee{app_name = AppName} = Bee} = State) ->
  PendingBees = lists:filter(fun(B) -> B#bee.status =:= pending end, bees:find_all_by_name(AppName)),
  case length(PendingBees) > 0 of
    true -> 
      {stop, already_pending_bees, Bee};
    false -> 
      Pid = node_manager:get_next_available(storage),
      Node = node(Pid),
      case apps:find_by_name(AppName) of
        App when is_record(App, app) ->
          % rpc:call(Node, ?STORAGE_SRV, pull_repos, [App, self()]);
          rpc:call(Node, ?STORAGE_SRV, fetch_or_build_bee, [App, self()]),
          {next_state, starting, State#state{bee = Bee#bee{storage_node = Node}, from = From}};
        _ ->
          io:format("Error?~n"),
          {stop, no_app, State#state{from = From}}
      end
  end;
  
pulling(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, pulling]),
  {next_state, pulling, State}.

squashing({pulled, _Info}, State) ->
  {next_state, starting, State};

squashing({error, Code}, State) ->
  io:format("Error: ~p~n", [Code]),
  {stop, Code, State};

squashing(Event, State) ->
  io:format("Received: ~p~n", [Event]),
  {next_state, squashing, State}.

starting({bee_built, Info}, #state{bee = #bee{app_name = AppName} = Bee} = State) ->
  % Strip off the last newline... stupid bash
  BeeSize = proplists:get_value(bee_size, Info),
  Sha = proplists:get_value(sha, Info),
  
  Pid = node_manager:get_next_available(node),
  Node = node(Pid),
  
  App = apps:find_by_name(AppName),
  {ok, P} = app_launcher_fsm:start_link(App, Node, Sha),
  app_launcher_fsm:launch(P, self()),
  
  NewBee = Bee#bee{bee_size = BeeSize, host_node = Node, commit_hash = Sha},
  io:format("bee_built: ~p for new hash: ~p~n", [NewBee, Sha]),
  {next_state, success, State#state{bee = NewBee}};

starting({error, could_not_pull_bee}, State) ->
  {stop, could_not_pull_bee, State};
  
starting(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, starting]),
  {next_state, launching, State}.

success({bee_started_normally, StartedBee, App}, #state{bee = #bee{commit_hash = Sha} = _Bee, from = From} = State) ->
  io:format("bee_started_normally: ~p for new hash: ~p~n", [StartedBee, Sha]),
  From ! {bee_started_normally, StartedBee#bee{commit_hash = Sha}, App#app{sha = Sha}},
  {stop, normal, State#state{bee = StartedBee}};
  
success(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, success]),
  {stop, normal, State}.
  
state_name(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
  {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, Bee) -> {next_state, NextBeeName, NextBee} |
%%                                   {next_state, NextBeeName,
%%                                     NextBee, Timeout} |
%%                                   {reply, Reply, NextBeeName, NextBee}|
%%                                   {reply, Reply, NextBeeName,
%%                                    NextBee, Timeout} |
%%                                   {stop, Reason, NewBee}|
%%                                   {stop, Reason, Reply, NewBee}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name BeeName is called to handle the event.
%%--------------------------------------------------------------------
state_name(Event, _From, State) ->
  Reply = ok,
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, BeeName, Bee) -> {next_state, NextBeeName,
%%                                                NextBee} |
%%                                          {next_state, NextBeeName,
%%                                                NextBee, Timeout} |
%%                                          {stop, Reason, NewBee}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(Event, BeeName, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, BeeName]),
  {next_state, BeeName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, BeeName,
%%                   Bee) -> {next_state, NextBeeName, NextBee} |
%%                             {next_state, NextBeeName, NextBee,
%%                              Timeout} |
%%                             {reply, Reply, NextBeeName, NextBee}|
%%                             {reply, Reply, NextBeeName, NextBee,
%%                              Timeout} |
%%                             {stop, Reason, NewBee} |
%%                             {stop, Reason, Reply, NewBee}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, BeeName, State) ->
  Reply = ok,
  {reply, Reply, BeeName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,BeeName,Bee)-> {next_state, NextBeeName, NextBee}|
%%                                     {next_state, NextBeeName, NextBee,
%%                                       Timeout} |
%%                                     {stop, Reason, NewBee}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({get_state}, _BeeName, State) ->
  {reply, State, State};
handle_info({stop}, _BeeName, State) ->
  {stop, normal, State};
handle_info(Info, BeeName, State) ->
  apply(?MODULE, BeeName, [Info, State]).
  % {next_state, BeeName, Bee}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, BeeName, Bee) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _BeeName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, BeeName, Bee, Extra) -> {ok, BeeName, NewBee}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, BeeName, State, _Extra) ->
  {ok, BeeName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------