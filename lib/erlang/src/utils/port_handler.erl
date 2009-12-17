-module (port_handler).
-export([
          start/4
        ]).

start(Cmd, WorkingDir, From, Opts) ->
  process_flag(trap_exit, true),
  spawn_link(fun() ->
    Port = open_port({spawn, Cmd}, [exit_status, {cd, file_utils:relative_path(WorkingDir)}|Opts]),
    port_loop(Port, Cmd, WorkingDir, From)
  end).

port_loop(Port, Cmd, WorkingDir, From) ->
  receive
    {Port, {data, Data}} ->
      From ! {data, Data},
      port_loop(Port, Cmd, WorkingDir, From);
	  {stop} -> 
      port_close(Port),
      From ! {port_closed, self(), 0},
	    exit(normal);
	  {'EXIT',Port,Reason} -> 
	    io:format("Port exited: ~p because ~p~n", [Port, Reason]),
	    exit({error, Reason});
	  {Port, {exit_status, 0}} -> 
      io:format("Port decided to exit for sane reasons~n"),
      (catch port_close(Port)),
	    exit(normal);
    {Port, {exit_status, Code}} ->
      io:format("Port decided to exit for some reason: ~p~n", [Code]),
	    exit({exit_status, Code});
	  Else ->
	    io:format("Port unknowingly received: ~p~n", [Else]),
	    port_loop(Port, Cmd, WorkingDir, From)
  end.
