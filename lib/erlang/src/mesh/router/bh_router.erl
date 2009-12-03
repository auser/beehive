%%%-------------------------------------------------------------------
%%% File    : bh_router.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec  3 01:03:25 PST 2009
%%%-------------------------------------------------------------------

-module (bh_router).

-include ("beehive.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) -> bh_router_sup:start_link(Args).

stop(_State) -> ok.
