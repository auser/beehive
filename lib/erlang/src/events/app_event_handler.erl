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

-define (LAUNCHERS_APP_TO_PID, 'launchers_app_to_pid').
-define (LAUNCHERS_PID_TO_APP, 'launchers_pid_to_app').
-define (UPDATERS_PID_TO_APP, 'updaters_pid_to_app').
-define (UPDATERS_APP_TO_PID, 'updaters_app_to_pid').

-define (ACTION_TIMEOUT, 10).
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
  
  Opts = [named_table, set],
  (catch ets:new(?UPDATERS_PID_TO_APP, Opts)),
  (catch ets:new(?UPDATERS_APP_TO_PID, Opts)),
  
  (catch ets:new(?LAUNCHERS_PID_TO_APP, Opts)),
  (catch ets:new(?LAUNCHERS_APP_TO_PID, Opts)),
  
  {ok, _TRef} = timer:send_interval(5000, flush_old_processes),
  
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
  handle_updating_app(App),
  {ok, State};

handle_event({app, restart, App}, State) ->
  handle_restart_app(App),
  {ok, State};
  
% Fired when the squashed app has not been found
handle_event({app, app_not_squashed, App}, State) ->
  ?LOG(info, "app_not_squashed yet: ~p", [App#app.name]),
  handle_updating_app(App),
  {ok, State};

% TODO: Refactor
handle_event({app, request_to_start_new_bee, Hostname}, State) ->
  app_manager:request_to_start_new_bee(Hostname),
  {ok, State};
  
handle_event({app, request_to_start_new_bee, App, Host, Sha}, State) ->
  ?LOG(info, "request_to_start_new_bee: ~p~n", [App]),
  handle_launch_app(App, Host, Sha),
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
handle_info({'EXIT', Pid, _Reason}, State) ->
  io:format("Pid exited: ~p~n", [Pid]),
  case ets:lookup(?UPDATERS_PID_TO_APP, Pid) of
    [{Pid, App, _Time}] ->
      ets:delete(?UPDATERS_PID_TO_APP, Pid),
      ets:delete(?UPDATERS_APP_TO_PID, App);
    _ -> 
      case ets:lookup(?LAUNCHERS_PID_TO_APP, Pid) of
        [{Pid, App, _Time}] ->
          ets:delete(?LAUNCHERS_PID_TO_APP, Pid),
          ets:delete(?LAUNCHERS_APP_TO_PID, App);
        _ -> true
      end
  end,
  {ok, State};
  
handle_info(flush_old_processes, State) ->
  lists:map(fun({Pid, App, Time}) ->
    case date_util:now_to_seconds() - Time > ?ACTION_TIMEOUT of
      false -> ok;
      true ->
        ets:delete(?UPDATERS_PID_TO_APP, Pid),
        ets:delete(?UPDATERS_APP_TO_PID, App)
    end
  end, ets:tab2list(?UPDATERS_PID_TO_APP)),
  lists:map(fun({Pid, App, Time}) ->
    case date_util:now_to_seconds() - Time > ?ACTION_TIMEOUT of
      false -> ok;
      true ->
        ets:delete(?LAUNCHERS_PID_TO_APP, Pid),
        ets:delete(?LAUNCHERS_APP_TO_PID, App)
    end
  end, ets:tab2list(?LAUNCHERS_PID_TO_APP)),
  {ok, State};

handle_info({bee_started_normally, #bee{commit_hash = Sha} = Bee, #app{name = AppName} = App}, State) ->
  % StartedBee#bee{commit_hash = Sha}, App#app{sha = Sha}
  ?LOG(debug, "app_event_handler got bee_started_normally: ~p, ~p", [Bee, App]),
  apps:transactional_save(fun() ->
    RealApp = apps:find_by_name(AppName),
    apps:save(RealApp#app{sha = Sha})
  end),
  bees:transactional_save(fun() ->
    RealBee = bees:find_by_id(Bee#bee.id),
    bees:save(RealBee#bee{lastresp_time = date_util:now_to_seconds()})
  end),
  ok = kill_other_bees(Bee),
  {ok, State};

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

handle_updating_app(App) ->  
  case ets:lookup(?UPDATERS_APP_TO_PID, App) of
    [{_App, _Pid, Time}] -> 
      ?LOG(info, "Cannot update app as there is already one in progress (timeout: ~p)", [date_util:now_to_seconds() - Time]),
      ok;
    _ ->
      {ok, P} = app_updater_fsm:start_link(App),
      Now = date_util:now_to_seconds(),
      app_updater_fsm:go(P, self()), 
      ets:insert(?UPDATERS_APP_TO_PID, {App, P, Now}),
      ets:insert(?UPDATERS_PID_TO_APP, {P, App, Now})
  end.

handle_restart_app(App) when is_record(App, app) -> app_manager:request_to_terminate_all_bees(App#app.name);
handle_restart_app(Name) -> app_manager:request_to_terminate_all_bees(Name).

handle_launch_app(App, Host, Sha) ->
  case ets:lookup(?LAUNCHERS_APP_TO_PID, App) of
    [{_App, _Pid, Time}] -> 
      ?LOG(info, "Cannot launch app as there is already one in progress (timeout: ~p)", [date_util:now_to_seconds() - Time]),
      ok;
    _ -> 
      {ok, P} = app_launcher_fsm:start_link(App, Host, Sha),
      Now = date_util:now_to_seconds(),
      app_launcher_fsm:launch(P, self()),
      ets:insert(?LAUNCHERS_APP_TO_PID, {App, P, Now}),
      ets:insert(?LAUNCHERS_PID_TO_APP, {P, App, Now})
  end.

% Kill off all other bees
kill_other_bees(#bee{app_name = Name, id = StartedId, commit_hash = StartedSha} = _StartedBee) ->
  case bees:find_all_by_name(Name) of
    [] ->
      ok;
    CurrentBees ->
      OtherBees = lists:filter(fun(B) -> B#bee.id =/= StartedId orelse B#bee.commit_hash =/= StartedSha end, CurrentBees),
      lists:map(fun(B) -> ?NOTIFY({bee, terminate_please, B}) end, OtherBees),
      ok
  end.
