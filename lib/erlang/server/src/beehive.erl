-module (beehive).
-include ("beehive.hrl").
-behaviour (application).

-export([start/0, start/2, stop/1, stop/0]).
-export ([start_phase/3]).

start()           -> start(normal, []).
start(Type, Args) ->  beehive_app:start(Type, Args).

stop()      -> stop([]).  
stop(State) ->  beehive_app:stop(State), ok.

start_phase(go, normal, _Args) -> ok.