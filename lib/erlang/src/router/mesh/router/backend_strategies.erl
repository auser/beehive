%%%-------------------------------------------------------------------
%%% File    : backend_strategies.erl
%%% Author  : Hemant Borole
%%% Description : Strategy to choose backend based
%%%               on a random selection.
%%%
%%% Created :  Mon Nov 23 14:02:36 PST 2009
%%%-------------------------------------------------------------------

-module(backend_strategies).

-export([random/1]).

random([]) -> [] ; %% Come back and update this,
                   %% throw an exception when emtpy?
random( Backends ) ->
  RandNum = random:uniform(length(Backends)),
  lists:nth(RandNum, Backends).


