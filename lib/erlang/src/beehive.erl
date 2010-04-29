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

-define (APPS, [os_mon, mnesia]).

-export([start/2, stop/1]).

start(_Type, Args) -> 
  io:format("Dir: ~p~n", [?BH_ROOT]),
  lists:foldr(fun(App, Acc) -> application:start(App), [App|Acc] end, [], ?APPS),
  beehive_sup:start_link(Args).

stop(State) -> 
  lists:foldr(fun(App) -> App:stop(State) end, ?APPS),
  ok.
