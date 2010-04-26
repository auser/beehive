%%%-------------------------------------------------------------------
%%% File    : babysitter_app.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec 24 15:07:11 PST 2009
%%%-------------------------------------------------------------------

-module (babysitter_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) -> 
  babysitter_sup:start_link(Args).

stop(_State) -> 
  io:format("Stopping babysitter_app...~n"),
  ok.
