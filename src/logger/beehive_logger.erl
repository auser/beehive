%%%-------------------------------------------------------------------
%%% File    : beehive_logger.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Mar  2 01:00:02 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_logger).
-behaviour(gen_server).
-include ("beehive_logger.hrl").

%% API
-export([start_link/1, stop/1, append/1, print/0, upread/1,
          error/1,error/2,
          info/1,info/2,
          truncate/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop(_Args) ->
  gen_server:call(?SERVER, stop),
  ok.

error(Msg) -> error(Msg, []).
error(Msg, Args) -> append(lists:flatten(io_lib:format(Msg, Args))).

info(Msg) -> info(Msg, []).
info(Msg, Args) -> append(lists:flatten(io_lib:format(Msg, Args))).

append(Log) -> gen_server:cast(?SERVER, {append, term_to_binary(Log)}).
upread(Fun) -> gen_server:cast(?SERVER, {upread, Fun}).

print() -> gen_server:cast(?SERVER, {print}).
truncate() -> gen_server:cast(?SERVER, truncate).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Conf) ->
  FileName = case application:get_env(beehive_logger, log_path) of
    {ok, undefined} -> get_log_from_config(Conf);
    { ok, Log } ->  Log;
    undefined -> get_log_from_config(Conf)
  end,
  
  io:format("Logging to file: ~p~n", [FileName]),
  
  case file:open(FileName, [read, write, raw, binary]) of
  {ok, Fd} ->
    {ok, Eof} = file:position(Fd, eof),
    file:position(Fd, bof),
    FilePos = position_fd(Fd, 0),
    maybe_warn(FilePos, Eof),
    {ok, Fd};
  {error, Reason} ->
    warn("Can't open ~p~n", [FileName]),
    {stop, Reason}
  end.
  

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------     
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({append, Log}, Fd) ->
  log_binary(Fd, Log),
  {noreply, Fd};
  
handle_cast({upread, Fun}, Fd) ->
  upread(Fd, Fun),
  {reply, Fd};

handle_cast(truncate, Fd) ->
    file:position(Fd, bof),
    file:truncate(Fd),
    {noreply, Fd};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, Fd) ->
  file:close(Fd).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
  
maybe_warn(FilePos, Eof) ->
    if
  FilePos == Eof ->
      ok;
  true ->
      warn("~w bytes truncated \n", 
     [Eof - FilePos])
    end.


position_fd(Fd, LastPos) ->
    case catch getint32(Fd) of
  Int when is_integer(Int) ->
      case file:read(Fd, Int) of
    {ok, B} when size(B) ==  Int ->
        position_fd(Fd, LastPos + 4 + Int);
    _ ->
        file:position(Fd, LastPos),
        file:truncate(Fd)
      end;
  _ ->
      file:position(Fd, LastPos),
      file:truncate(Fd),
      LastPos
    end.

log_binary(Fd, Bin) ->
    Sz = size(Bin),
    case file:write(Fd, [i32(Sz), Bin]) of
  ok ->
      ok;
  {error, Reason} ->
      warn("Cant't write logfile ~p ", [Reason]),
      {error, Reason}
    end.


warn(Fmt, As) ->
    io:format(user, "beehive_logger: " ++ Fmt, [As]).


upread(Fd, Fun) ->    
    {ok, _Curr} = file:position(Fd, cur),
    file:position(Fd, bof),
    upread(Fd, get_term(Fd), Fun).

upread(_Fd, {'EXIT', _}, _Fun) ->
    ok;
upread(Fd, Term, Fun) ->
    Fun(Term),
    upread(Fd, catch get_term(Fd), Fun).


get_term(Fd) ->
    I = getint32(Fd),
    {ok, B} = file:read(Fd, I),
    binary_to_term(B).


i32(B) when is_binary(B) ->
    i32(binary_to_list(B, 1, 4));
i32([X1, X2, X3, X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4;
i32(Int) when integer(Int) ->
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255].
         
getint32(F) ->
    {ok, B} = file:read(F, 4),
    i32(B).
    

get_log_from_config(Conf) ->
  case config:get(log_path) of
    {error, _} -> 
      case proplists:get_value(log_path, Conf) of
        {ok, L} -> L;
        _ -> get_log_from_environment()
      end;
    {ok, V} -> V
  end.
  
get_log_from_environment() ->
  case os:getenv("LOG_PATH") of 
      false -> "logs/beehive_logger.log";
      P -> P
  end.  