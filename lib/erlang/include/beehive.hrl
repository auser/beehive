-define (?ROOT_DIR_PREFIX, "/opt/beehive").

-define (FMT_MSG (Msg, Args), lists:flatten([?MODULE, ?LINE, io_lib:format(Msg, Args)])).
-define (INFO (Msg, Args),    beehive_logger:info(Msg, Args)).
-define (DEBUG (Msg, Args),   beehive_logger:debug(Msg, Args)).
-define (ERROR (Msg, Args),   beehive_logger:error(Msg, Args)).

-define (LOG_MESSAGE (Message, Args), io_lib:fwrite("~p~p~n", [Message, Args])).

-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).
