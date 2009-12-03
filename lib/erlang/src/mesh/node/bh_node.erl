%%%-------------------------------------------------------------------
%%% File    : bh_node.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec  3 01:03:04 PST 2009
%%%-------------------------------------------------------------------

-module (bh_node).

-include ("beehive.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) -> bh_node_sup:start_link(Args).

stop(_State) -> ok.
