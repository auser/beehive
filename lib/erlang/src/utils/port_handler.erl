-module (port_handler).
-include ("common.hrl").
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
	  {stop, StopCommand} -> 
	    _ClosePort = open_port({spawn, StopCommand}, [{packet, 4}, in, {cd, WorkingDir}]),
      % port_close(Port), port_close(ClosePort),
	    ok;
	  {'EXIT',Port,_Reason} -> 
	    ok;
	  {Port, {exit_status, 0}} -> 
      % io:format("Port decided to exit for sane reasons~n"),
      % (catch port_close(Port)),
	    From ! {port_closed, 0};
    {Port, {exit_status, Code}} ->
      % io:format("Port decided to exit for some reason: ~p~n", [Code]),
	    From ! {port_exited, Code};
	  Else ->
	    io:format("Port unknowingly received: ~p~n", [Else]),
	    port_loop(Port, Cmd, WorkingDir, From)
  end.
