-module (beehive_app).
-include ("beehive.hrl").
-behaviour (application).

-export([start/0, start/2, stop/1, stop/0]).

start() ->
  start(normal, []).

start(Type, Args) ->  
  beehive_sup:start(Type, Args).

stop() -> stop([]).
  
stop(State) -> 
  beehive_sup:stop(State),
  ok.