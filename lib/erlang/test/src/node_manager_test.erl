-module (node_manager_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

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
        fun test_starting_node_manager/0,
        fun test_is_a_type/0,
        fun test_get_node_of_type/0
      ]
    }
  }.

test_starting_node_manager() ->
  ok.

test_is_a_type() ->
  ?assert(node_manager:is_a(router)).

test_get_node_of_type() ->
  ?assertEqual(node_manager:get_routers(), [erlang:whereis(node_manager)]),
  ?assertEqual(node_manager:get_nodes(), []),
  ?assertEqual(node_manager:get_storage(), []).