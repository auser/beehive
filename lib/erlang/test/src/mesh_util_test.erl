-module (mesh_util_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  pg2:create(one),
  pg2:create(two),
  ok.
  
teardown(_X) ->
  pg2:delete(one), pg2:delete(two),
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
  OnePids = lists:map(
    fun(_X) -> Pid = spawn(fun() -> timer:sleep(10) end), pg2:join(one, Pid), Pid end, lists:seq(1,10)),
  TwoPids = lists:map(fun(_X) -> Pid = spawn(fun() -> timer:sleep(10) end), pg2:join(two, Pid), Pid end, lists:seq(11,20)),
  {ok, OnePid} = mesh_util:get_random_pid(one),
  ?assert(lists:member(OnePid, OnePids)),
  {ok, TwoPid} = mesh_util:get_random_pid(two),
  ?assert(lists:member(TwoPid, TwoPids)),
  ok.

test_get_best_pid() ->
  ok.