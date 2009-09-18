-module (app_registry_app).
-include ("beehive.hrl").
-behaviour (application).

-export([start/0, start/2, stop/1, stop/0]).

start() ->
  start(normal, []).

start(_Type, Args) ->  
  app_registry_sup:start_link(Args).

stop() -> stop([]).
  
stop(State) -> 
  app_registry_sup:stop(State),
  ok.