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

start(_Type, Args) -> 
  lists:map(fun(App) ->
    io:format("---> starting ~p~n", [App]),
    App:start()
  end, [crypto]),
  babysitter:start_link(),
  bh_node_sup:start_link(Args).

stop(State) -> 
  io:format("Stopping beehive bee server...~n"),
  lists:map(fun(App) ->
    io:format("---> stopping ~p~n", [App]),
    App:stop(State)
  end, [crypto]).
