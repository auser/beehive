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

-define (APPS, [os_mon, mnesia, beehive]).

-export([start/2, stop/1]).

start(_Type, _Args) -> 
  io:format("Dir: ~p~n", [?BH_ROOT]),
  lists:foldr(fun(App) -> application:start(App) end, ?APPS).

stop(State) -> 
  lists:foldr(fun(App) -> App:stop(State) end, ?APPS),
  ok.
