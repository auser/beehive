%%%-------------------------------------------------------------------
%%% File    : beehive_dashboard.erl
%%% Author  : Ari Lerner
%%% Description : Dashboard server
%%%
%%% Created :  Thu Dec  3 01:05:11 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_dashboard).
-include ("beehive.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) -> beehive_dashboard_sup:start_link(Args).

stop(_State) -> ok.
