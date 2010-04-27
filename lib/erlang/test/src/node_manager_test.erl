-module (node_manager_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  event_manager:start_link(),
  node_manager:start_link(),
  ok.
  
teardown(_X) ->
  node_manager:stop(),
  event_manager:stop(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_starting_node_manager/0
      ]
    }
  }.

test_starting_node_manager() ->
  ok.