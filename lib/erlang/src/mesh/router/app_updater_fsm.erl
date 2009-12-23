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
  {ok, pulling, #bee{app_name = AppName}}.

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
pulling({go, _From}, #bee{app_name = AppName} = Bee) ->
  PendingBees = lists:filter(fun(B) -> B#bee.status =:= pending end, bees:find_all_by_name(AppName)),
  case length(PendingBees) > 0 of
    true -> 
      {stop, already_pending_bees, Bee};
    false -> 
      Node = node_manager:get_next_available_storage(),
      case apps:find_by_name(AppName) of
        App when is_record(App, app) ->
          % rpc:call(Node, ?STORAGE_SRV, pull_repos, [App, self()]);
          rpc:call(Node, ?STORAGE_SRV, build_bee, [App, self()]),
          {next_state, starting, Bee#bee{storage_node = Node}};
        _ ->
          io:format("Error?~n"),
          {error, no_app, Bee}
      end
  end;
  
pulling(Event, Bee) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, pulling]),
  {next_state, pulling, Bee}.

squashing({pulled, _Info}, Bee) ->
  {next_state, starting, Bee};

squashing({error, Code}, Bee) ->
  io:format("Error: ~p~n", [Code]),
  {stop, Code, Bee};

squashing(Event, Bee) ->
  io:format("Received: ~p~n", [Event]),
  {next_state, squashing, Bee}.

starting({bee_built, Info}, #bee{app_name = AppName} = Bee) ->
  % Return is bee_size dir_size
  % Strip off the last newline... stupid bash
  BeeSize = proplists:get_value(bee_size, Info),
  DirSize = proplists:get_value(dir_size, Info),
  Sha = proplists:get_value(sha, Info),
  
  Node = node_manager:get_next_available_host(),
  
  App = apps:find_by_name(AppName),
  io:format("app_launcher_fsm for node: ~p~n", [Node]),
  {ok, P} = app_launcher_fsm:start_link(App, Node, Sha),
  app_launcher_fsm:launch(P, self()),
  
  NewBee = Bee#bee{bee_size = BeeSize, dir_size = DirSize, host_node = Node, commit_hash = Sha},
  io:format("bee_built: ~p for new hash: ~p~n", [NewBee, Sha]),
  {next_state, success, NewBee};

starting({error, could_not_pull_bee}, Bee) ->
  {stop, could_not_pull_bee, Bee};
starting(Event, Bee) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, starting]),
  {next_state, launching, Bee}.

success({bee_started_normally, StartedBee, App}, #bee{app_name = Name, commit_hash = Sha} = Bee) ->
  io:format("bee_started_normally: ~p for new hash: ~p~n", [StartedBee, Sha]),
  
  case bees:find_all_by_name(Name) of
    [] ->
      apps:create(App#app{sha = Sha}),
      bees:save(StartedBee#bee{commit_hash = Sha}),
      {stop, normal, Bee};
    CurrentBees ->
      lists:map(fun(B) ->
        case B#bee.id =:= StartedBee#bee.id of
          true -> skip;
          false ->
            ?LOG(debug, "Terminating old bee: ~p~n", [B]),
            node_manager:request_to_terminate_bee(B),
            bees:save(B#bee{status = terminated})
        end
      end, CurrentBees),
      
      apps:create(App#app{sha = Sha}),
      bees:save(StartedBee#bee{commit_hash = Sha}),
      
      {stop, normal, Bee}
  end;

success(Event, Bee) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, success]),
  {stop, normal, Bee}.
  
state_name(Event, Bee) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
  {next_state, state_name, Bee}.

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
state_name(Event, _From, Bee) ->
  Reply = ok,
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
  {reply, Reply, state_name, Bee}.

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
handle_event(Event, BeeName, Bee) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, BeeName]),
  {next_state, BeeName, Bee}.

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
handle_sync_event(_Event, _From, BeeName, Bee) ->
  Reply = ok,
  {reply, Reply, BeeName, Bee}.

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
handle_info({get_state}, _BeeName, Bee) ->
  {reply, Bee, Bee};
handle_info({stop}, _BeeName, Bee) ->
  {stop, normal, Bee};
handle_info(Info, BeeName, Bee) ->
  apply(?MODULE, BeeName, [Info, Bee]).
  % {next_state, BeeName, Bee}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, BeeName, Bee) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _BeeName, _Bee) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, BeeName, Bee, Extra) -> {ok, BeeName, NewBee}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, BeeName, Bee, _Extra) ->
  {ok, BeeName, Bee}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
