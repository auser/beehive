% 
%  dashboard_event_handler.erl
%  beehive
%  
%  Created by Ari Lerner and David Guttman
%  Copyright 2010 Ari Lerner. All rights reserved.
% 
-module (dashboard_event_handler).
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
handle_event({user, Atom, User}, State) ->
  Msg = [{context, user}, {event, Atom}, {user, [{email, User#user.email}, {token, User#user.token}]}],
  beehive_dashboard_srv:send_message_to_all_websockets(Msg),
  {ok, State};
handle_event({app, Event, App}, State) when is_record(App, app) ->
  Msg = [
    {context, app}, 
    {event, Event}, 
    {app, [
      {name, App#app.name}, 
      {url, App#app.url}, 
      {updated_at, App#app.updated_at}
      ]
    }],
  beehive_dashboard_srv:send_message_to_all_websockets(Msg),
  {ok, State};
handle_event({bee, Event, Bee}, State) when is_record(Bee, bee) ->
  Msg = [
    {context, bee}, 
    {event, Event}, 
    {bee, bee_to_proplist(Bee)}
    ],
  beehive_dashboard_srv:send_message_to_all_websockets(Msg),
  {ok, State};
handle_event({bee, closing_stats, _Bee, _Stats}, State) ->
  {ok, State};
handle_event(Event, State) ->
  erlang:display({dashboard_event_handler, Event}),
  % beehive_dashboard_srv:send_message_to_all_websockets(Event),
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
bee_to_proplist(Bee) ->
  [
    {host, web_utils:ip_to_list(Bee#bee.host)}, 
    {port, Bee#bee.port}, 
    {app_name, Bee#bee.app_name}
  ].