%%%-------------------------------------------------------------------
%%% File    : log_event_handler.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Nov  2 12:17:32 PST 2009
%%%-------------------------------------------------------------------

-module(log_event_handler).
-include ("common.hrl").
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {
  filename,
  log_handle
}).
%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
  FileName = apps:search_for_application_value(log_path, "/tmp/router.log", beehive),
  % Get the full path for the file
  FullFilepath = case (catch file_utils:abs_or_relative_filepath(FileName)) of
    {error, _} -> "router.log";
    P -> P
  end,
  
  {ok, Fd} = file:open(FullFilepath, [append]),
  
  {ok, #state{filename = FullFilepath, log_handle = Fd}}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%--------------------------------------------------------------------
handle_event({log, LogLevel, LogFormat, LogArgs}, State) ->
  write(LogLevel, io_lib:format(LogFormat, LogArgs), State),
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
terminate(_Reason, #state{log_handle = LH} = _State) ->
  file:close(LH),
  ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
% INTERNAL FUNCTIONS
write(Level, Message, #state{log_handle = Fd} = _State) ->
  % Check levels here, eventually
  Msg = io_lib:format("[~s] [~s] ~s\r~n\r", [httpd_util:rfc1123_date(), Level, Message]),
  io:format("~s", [Msg]),     % write to console 
  io:format(Fd, "~s", [Msg]). % write to file
  