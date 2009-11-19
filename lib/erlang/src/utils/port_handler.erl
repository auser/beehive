-module (port_handler).
-include ("common.hrl").
-export([
          start/3
        ]).

% P = port_handler:start("thin -R beehive.ru --port 5000 start", "/Users/auser/Development/erlang/mine/router/test/fixtures/apps").

start(Cmd, WorkingDir, From) ->
  process_flag(trap_exit, true),
  spawn_link(fun() ->
    Port = open_port({spawn, Cmd}, [{packet, 4}, exit_status, nouse_stdio, {cd, WorkingDir}]),
    port_loop(Port, Cmd, WorkingDir, From)
  end).

port_loop(Port, Cmd, WorkingDir, From) ->
  receive
	  {stop, StopCommand} -> 
	    io:format("Stopping port: ~p~n", [StopCommand]),
	    _ClosePort = open_port({spawn, StopCommand}, [{packet, 4}, in, {cd, WorkingDir}]),
      % port_close(Port), port_close(ClosePort),
	    ok;
	  {'EXIT',Port,_Reason} -> 
	    ok;
	  {Port, {exit_status, 0}} -> 
	    io:format("Port decided to exit for sane reasons~n"),
	    From ! {port_closed, Port};
      % port_close(Port);
    {Port, {exit_status, Code}} ->
	    io:format("Port decided to exit for some reason: ~p", [Code]);
      % port_close(Port);
	  Else ->
	    io:format("Port received: ~p~n", [Else]),
	    port_loop(Port, Cmd, WorkingDir, From)
  end.
