-module (beehive_router_srv_tests).
-include_lib("eunit/include/eunit.hrl").

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
        fun test_startup/0
      ]
    }
  }.

test_startup() ->
  erlang:display({beehive_router_srv_tests, test_startup}),
  passed.