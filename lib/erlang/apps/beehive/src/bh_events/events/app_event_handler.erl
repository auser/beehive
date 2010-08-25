%%%-------------------------------------------------------------------
%%% File    : app_event_handler.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Mon Nov  2 12:13:56 PST 2009
%%%-------------------------------------------------------------------

-module (app_event_handler).

-include ("beehive.hrl").
-include ("common.hrl").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-record (state, {}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
  ?QSTORE:start_link(?WAIT_DB),
  process_flag(trap_exit, true),
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
handle_event({app, updated, App}, State) ->
  % We want the app to rebuild, so we'll remove the sha and force it to rebuild
  handle_update_app(App),
  {ok, State};

handle_event({app, restart, App}, State) ->
  handle_restart_app(App),
  {ok, State};

handle_event({app, expand, App}, State) ->
  handle_expand_app(App),
  {ok, State};

% Fired when the squashed app has not been found
handle_event({app, app_not_squashed, App}, State) ->
  handle_update_app(App),
  {ok, State};

handle_event({app, updated_revision, App}, State) ->
  handle_update_app(App),
  {ok, State};

% THIS NEEDS TO BE FEDERATED...
handle_event({app, request_to_start_new_bee, App}, State) when is_record(App, app) ->
  app_manager:request_to_start_new_bee_by_app(App),
  {ok, State};
handle_event({app, request_to_start_new_bee, Hostname}, State) ->
  app_manager:request_to_start_new_bee_by_name(Hostname),
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
handle_info({bee_terminated, _Bee}, State) ->
  {ok, State};

handle_info(Info, State) ->
  ?LOG(debug, "app_event_handler info: ~p", [Info]),
  % apps:create(App),
  % bees:save(StartedBee);
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
handle_restart_app(App) when is_record(App, app) ->
  app_manager:terminate_app_instances(App#app.name);
handle_restart_app(Name) -> app_manager:terminate_app_instances(Name).

handle_expand_app(App) -> app_manager:request_to_expand_app(App).

handle_update_app(App) -> app_manager:request_to_update_app(App).
