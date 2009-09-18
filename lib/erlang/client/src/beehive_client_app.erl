-module (beehive_client_app).
-include ("beehive.hrl").
-behavior(application).

%% application callbacks
-export([start/2, stop/1]).

start(_Type, Args) ->
  beehive_client_sup:start_link(Args).

stop(_State) ->
    ok.