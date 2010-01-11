%%%-------------------------------------------------------------------
%%% File    : bee_event_handler.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov  8 17:02:19 PST 2009
%%%-------------------------------------------------------------------

-module (bee_event_handler).

-include ("beehive.hrl").
-include ("common.hrl").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
  {ok, 500}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%--------------------------------------------------------------------  
handle_event({bee, create, Bee}, State) when is_record(Bee, bee) ->
  bees:create(Bee),
  {ok, State};

% Caught when a connection has been established to the bee
handle_event({bee, used, Backend}, State) when is_record(Backend, bee) ->
  bh_bee_stats_srv:bee_stat({request_begin, Backend#bee.id}),
  {ok, State};

% Caught when a connection has disconnected
handle_event({bee, ready, Bee}, State) when is_record(Bee, bee) ->
  bee_srv:maybe_handle_next_waiting_client(Bee#bee.app_name),
  bees:transactional_save(fun() ->
    RealBee = bees:find_by_id(Bee#bee.id),
    bees:update(RealBee#bee{lastresp_time = date_util:now_to_seconds()})
  end),
  {ok, State};

% Handle generic status update
handle_event({bee, update_status, Bee, Status}, State) ->
  bees:transactional_save(fun() ->
    RealBee = bees:find_by_id(Bee#bee.id),
    bees:update(RealBee#bee{status = Status})
  end),
  {ok, State};

% Handle terminate bee
handle_event({bee, terminate_please, Bee}, State) ->
  node_manager:request_to_terminate_bee(Bee),
  {ok, State};

% Caught when a bee is marked as down
handle_event({bee, bee_down, Bee}, State) ->
  bees:transactional_save(fun() ->
    RealBee = bees:find_by_id(Bee#bee.id),
    bees:update(RealBee#bee{status = down})
  end),
  {ok, State};

% Caught when a bee is terminated
handle_event({bee, bee_terminated, Bee}, State) when is_record(Bee, bee) ->
  RealBee = bees:find_by_id(Bee#bee.id),
  ?LOG(debug, "{bee, bee_terminated, ~p}", [RealBee]),
  bees:update(RealBee#bee{status = terminated}),
  {ok, State};

% Catch a cannot connect error
handle_event({bee, cannot_connect, Id}, State) ->
  % bees:transactional_save(fun() ->
  RealBee = bees:find_by_id(Id),
  ?LOG(debug, "{bee, cannot_connect, ~p}", [RealBee]),
  bees:update(RealBee#bee{status = down}),
  % end),
  {ok, State};

handle_event({bee, closing_stats, #bee{id = Id} = Backend, StatsProplist}, State) ->
  % When the bee socket connection closes, let's save this data
  case proplists:get_value(socket, StatsProplist) of
    undefined -> ok;
    Val -> bh_bee_stats_srv:bee_stat({socket, Id, Val})
  end,
  case proplists:get_value(elapsed_time, StatsProplist) of
    undefined -> ok;
    Time -> bh_bee_stats_srv:bee_stat({elapsed_time, Id, Time})
  end,
  bh_bee_stats_srv:bee_stat({request_complete, Id}),
  
  bee_srv:maybe_handle_next_waiting_client(Backend#bee.app_name),
  {ok, State};
  
handle_event(_Event, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
% INTERNAL METHODS
