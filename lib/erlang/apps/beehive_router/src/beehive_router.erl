%%%-------------------------------------------------------------------
%%% File    : bh_router.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Thu Dec  3 01:03:25 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_router).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
  beehive_router_sup:start_link(Args).

stop(_State) -> ok.
