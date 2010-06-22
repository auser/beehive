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
  log_level,
  log_handle
}).

% Log levels
%   0 - quiet (no logs)
%   1 - error messages only
%   2 - verbose
%   3 - full logs with debugging messages
%   4 - heavy logging with speed
-ifdef (debug). 
-define (LOG_LEVELS, [info, debug, error, benchmark]).
-else.
-define (LOG_LEVELS, [info, debug, error]).
-endif.

-define (QUIET, 0).
-define (VERBOSE, 1).
-define (DEBUG, 2).
-define (BENCHMARK, 3).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
  LogPath  = misc_utils:to_list(config:search_for_application_value(log_path, ?BEEHIVE_DIR("logs"))),
  LogLevel = misc_utils:to_list(config:search_for_application_value(log_level, ?VERBOSE)),
  % Get the full path for the file
  LogName1 = misc_utils:to_list(config:search_for_application_value(node_type, node)),
  
  LogHandle = make_log(LogPath, LogName1),
  
  {ok, #state{log_handle = LogHandle, log_level = LogLevel}}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%--------------------------------------------------------------------
handle_event({log, LogLevel, LogFormat, LogArgs, File, Line}, State) ->
  NewState = write(LogLevel, File, Line, io_lib:format(LogFormat, LogArgs), State),
  {ok, NewState};
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
terminate(_Reason, #state{log_handle = Handle} = _State) ->
  lists:map(fun(_, Fd) -> file:close(Fd) end, [Handle]),
  ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
% INTERNAL FUNCTIONS

ensure_logfile_exists(FullFilepath) ->
  case (catch file:open(FullFilepath, [append])) of
    {ok, F} -> F;
    _E -> 
      FullPath = filename:join([filename:absname("./"), filename:basename(FullFilepath)]),
      filelib:ensure_dir(filename:dirname(FullPath)),
      {ok, F} = file:open(FullPath, [write]),
      F
  end.

write(Level, _File, _Line, Message, #state{log_handle = Handle, log_level = LogLevel} = State) ->
  Msg = io_lib:format("[~s] [~p] ~s\r\n", [httpd_util:rfc1123_date(), Level, Message]),
  write_to_sasl(LogLevel, Handle, Msg),
  case LogLevel of
    0 -> ok;
    _ -> write_to_console(Msg)
  end,
  State.

write_to_console(Msg) -> io:format("~s", [Msg]).
write_to_sasl(Level, Fd, Msg) ->
  case Level of
    ?QUIET -> ok;
    _ -> io:format(Fd, "~s", [Msg])
  end.

make_log(LogPath, LogName) ->
  FullFilepath = filename:join([LogPath, lists:append([LogName, ".log"])]),
  ensure_logfile_exists(FullFilepath).
