-module (mesh_util_test).
-include_lib("eunit/include/eunit.hrl").

-export ([loop_send/3]).

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_get_random_pid/0,
        fun test_get_best_pid/0
      ]
    }
  }.

test_get_random_pid() ->
  reset(),
  Parent = self(),
  OnePids = lists:map(fun(X) -> test_send(one, Parent, X) end, lists:seq(1, 10)),
  TwoPids = lists:map(fun(X) -> test_send(two, Parent, X) end, lists:seq(1, 10)),
  {ok, OnePid} = mesh_util:get_random_pid(one),
  ?assert(lists:member(OnePid, OnePids)),
  {ok, TwoPid1} = mesh_util:get_random_pid(two),
  ?assert(lists:member(TwoPid1, TwoPids)),
  {ok, TwoPid2} = mesh_util:get_random_pid(two),
  ?assert(lists:member(TwoPid2, TwoPids)),
  ?assertEqual({error, {no_process, unknown}}, mesh_util:get_random_pid(unknown)).

test_get_best_pid() ->
  reset(),
  Parent = self(),
  OnePids = lists:map(fun(X) -> test_send(one, Parent, X) end, lists:seq(1, 3)),  
  [ Pid ! {hello, "World"} || Pid <- OnePids ],
  {ok, OnePid} = mesh_util:get_best_pid(one),
  ?assert(lists:member(OnePid, OnePids)),
  ?assertEqual({error, {no_process, unknown}}, mesh_util:get_best_pid(unknown)),
  [ Pid ! stop || Pid <- OnePids ].
  
% Internal
reset() ->
  pg2:delete(one), pg2:delete(two),
  pg2:create(one), pg2:create(two).

test_send(Group, Parent, Count) ->
  Pid = spawn(?MODULE, loop_send, [Group, Parent, Count]),
  pg2:join(Group, Pid),
  Pid.

loop_send(_, _, 0) -> ok;
loop_send(Group, Parent, Count) ->
  receive
    stop -> ok;
    X -> 
      self() ! {self(), X}, self() ! {self(), X},
      loop_send(Group, Parent, Count)
  end.