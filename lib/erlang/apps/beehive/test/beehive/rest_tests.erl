-module (rest_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup([{node_type, beehive_router}]),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_connectable/0
      ]
    }
  }.

test_connectable() ->
  erlang:display({hi}),
  passed.