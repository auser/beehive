%%%-------------------------------------------------------------------
%%% File    : backend_event_handler.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov  8 17:02:19 PST 2009
%%%-------------------------------------------------------------------

-module (backend_event_handler).

-include ("router.hrl").
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
handle_event({backend, used, Backend}, State) ->
  stats_srv:backend_stat({request_begin, Backend#backend.id}),
  {ok, State};
  
handle_event({backend, ready, Backend}, State) ->
  backend_srv:maybe_handle_next_waiting_client(Backend#backend.app_name),
  {ok, State};

handle_event({backend, backend_down, Backend}, State) ->
  backend:update(Backend#backend{status = down}),
  {ok, State};

handle_event({backend, cannot_connect, Backend}, State) ->
  backend:update(Backend#backend{status = down}),
  {ok, State};

handle_event({backend, closing_stats, #backend{id = Id} = Backend, StatsProplist}, State) ->
  % When the backend socket connection closes, let's save this data
  case proplists:get_value(socket, StatsProplist) of
    undefined -> ok;
    Val -> stats_srv:backend_stat({socket, Id, Val})
  end,
  case proplists:get_value(elapsed_time, StatsProplist) of
    undefined -> ok;
    Time -> stats_srv:backend_stat({elapsed_time, Id, Time})
  end,
  stats_srv:backend_stat({request_complete, Id}),
  
  backend_srv:maybe_handle_next_waiting_client(Backend#backend.app_name),
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
