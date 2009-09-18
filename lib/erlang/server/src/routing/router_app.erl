-module (router_app).
-include ("beehive.hrl").
-behaviour (application).

-export([start/0, start/2, stop/1, stop/0]).

start() ->
  start(normal, []).

start(_Type, Args) ->  
  router_sup:start_link(Args).

stop() -> stop([]).
  
stop(State) -> 
  router_sup:stop(State),
  ok.