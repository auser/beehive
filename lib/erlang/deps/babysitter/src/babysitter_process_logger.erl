%%%-------------------------------------------------------------------
%%% File    : babysitter_process_logger.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec 24 16:08:31 PST 2009
%%%-------------------------------------------------------------------

-module (babysitter_process_logger).

-compile(inline).

-export([listener/1, start/1, write/2]).

start(Filename) ->
    Pid = spawn(?MODULE, listener, [Filename]),
    register(?MODULE, Pid).

listener(Filename) ->
    case file:open(Filename, [append, raw]) of
      {ok, Fd} -> listener(Fd, 0, []);
      {error, Reason} -> error_logger:error_msg(Reason)
    end.

listener(Fd, N, [] = SoFar) ->
    receive
      {data, Data} -> listener(Fd, N + 1, [Data]);
      {count, From} ->
          From ! {count, N}, listener(Fd, N, SoFar);
      stop -> ok;
      Msg ->
          io:format("Unexpected message: ~p~n", [Msg]),
          listener(Fd, N, SoFar)
    end;
listener(Fd, N, SoFar) ->
    receive
      {data, Data} -> listener(Fd, N + 1, [Data | SoFar]);
      {count, From} ->
          From ! {count, N}, listener(Fd, N, SoFar);
      stop -> ok;
      Msg ->
          io:format("Unexpected message: ~p~n", [Msg]),
          listener(Fd, N, SoFar)
      after 0 -> flush(Fd, N, SoFar)
    end.

flush(Fd, N, Data) ->
    file:write(Fd, lists:reverse(Data)),
    listener(Fd, N, []).

write(_Data, 0) -> ok;
write(Data, N) ->
    (?MODULE) ! {data, Data}, write(Data, N - 1).
