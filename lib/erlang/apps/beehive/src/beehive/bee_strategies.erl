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

random([]) -> [];
random(Backends) -> random_sort(Backends, []).

random_sort([], Acc) -> Acc;
random_sort(Bees, Acc) ->
  RandNum = random:uniform(length(Bees)),
  Bee = lists:nth(RandNum, Bees),
  NewBees = lists:delete(Bee, Bees),
  random_sort(NewBees, [Bee|Acc]).

% Get the least loaded bee
least_loaded(Backends) ->
  ListsOfBees = lists:map(fun(B) ->
    #bee_stat{current = CurrentReq} = case bh_bee_stats_srv:bee_dump(B#bee.id) of
      [{_Name, Q}|_] -> Q;
      _ -> bh_bee_stats_srv:new_bee_stat()
    end,
    {CurrentReq, B}
  end, Backends),
  SortedWithLoad = lists:sort(fun({A, _},{B, _}) -> A < B end, ListsOfBees),
  lists:map(fun({_Load, Bee}) -> Bee end, SortedWithLoad).