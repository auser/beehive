%%%-------------------------------------------------------------------
%%% File    : beehive.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Oct  8 18:29:29 PDT 2009
%%%-------------------------------------------------------------------

-module (beehive).
-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) -> 
  io:format("Dir: ~p~n", [?BH_ROOT]),
  beehive_sup:start_link(Args).

stop(State) -> 
  io:format("Stopping beehive...~n"),
  lists:map(fun(App) ->
    io:format("---> stopping ~p~n", [App]),
    App:stop(State)
  end, [os_mon, mnesia]),
  ok.
