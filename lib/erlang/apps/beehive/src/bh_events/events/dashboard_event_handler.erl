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

-define (DASHBOARD_EVENTS_TABLE, dashboard_event_handler_table).

%% API
-export ([get_latest_events/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {
  last_trans  = 0
}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
get_latest_events() ->
  Ret = ets:tab2list(?DASHBOARD_EVENTS_TABLE),
  prune_table(?DASHBOARD_EVENTS_TABLE, lists:map(fun({K,_}) -> K end, Ret)),
  Ret.
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
  ?DASHBOARD_EVENTS_TABLE = ets:new(?DASHBOARD_EVENTS_TABLE, [public,named_table]),
  {ok, #state{}}.

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
  {ok, handle_msg(Msg, State)};
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
  {ok, handle_msg(Msg, State)};
handle_event({bee, Event, Bee}, State) when is_record(Bee, bee) ->
  Msg = [
    {context, bee}, 
    {event, Event}, 
    {bee, bee_to_proplist(Bee)}
    ],
  {ok, handle_msg(Msg, State)};
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
  true = ets:delete(?DASHBOARD_EVENTS_TABLE),
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

handle_msg(_Msg, State) ->
  % TransId = next_trans(LastTrans),
  % ets:insert(?DASHBOARD_EVENTS_TABLE, [{TransId, Msg}]),
  % beehive_dashboard_srv:send_message_to_all_websockets(Msg),
  % State#state{last_trans = TransId}.
  State.

% So that we can get a unique id for each communication
next_trans(I) when I < 268435455 -> I+1;
next_trans(_) -> 1.

% Prune the table
prune_table(Tab, Keys) ->
  true = ets:safe_fixtable(Tab, true),
  ok = prune_table(Tab, Keys, ets:first(Tab)),
  true = ets:safe_fixtable(Tab, false).

prune_table(_Tab, _Keys, '$end_of_table') -> ok;
prune_table(Tab, Keys, Key) ->
  ets:match_delete(Tab, {Key, '_'}),
  prune_table(Tab, Keys, ets:next(Tab, Key)).
