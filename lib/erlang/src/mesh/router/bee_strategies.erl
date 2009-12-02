%%%-------------------------------------------------------------------
%%% File    : bee_strategies.erl
%%% Author  : Hemant Borole
%%% Description : Strategy to choose bee based
%%%               on a random selection.
%%%
%%% Created :  Mon Nov 23 14:02:36 PST 2009
%%%-------------------------------------------------------------------

-module(bee_strategies).
-include ("beehive.hrl").

-export([
  random/1,
  least_loaded/1
]).

random([]) -> [] ; %% Come back and update this,
                   %% throw an exception when emtpy?
random( Backends ) ->
  RandNum = random:uniform(length(Backends)),
  lists:nth(RandNum, Backends).

least_loaded(Backends) ->
  ListsOfBees = lists:map(fun(B) ->
    #bee_stat{current = CurrentReq} = case stats_srv:bee_dump(B#bee.id) of
      [{_Name, Q}|_] -> Q;
      _ -> stats_srv:new_bee_stat()
    end,
    {CurrentReq, B}
  end, Backends),
  SortedBees = lists:sort(fun({A, _},{B, _}) -> A < B end, ListsOfBees),
  {_, Bee} = hd(SortedBees),
  Bee.
  