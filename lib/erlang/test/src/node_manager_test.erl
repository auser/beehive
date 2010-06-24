-module (node_manager_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  event_manager:start_link(),
  node_manager:start_count(2),
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
        fun test_is_a_type/0,
        fun test_get_node_of_type/0,
        fun test_sort_servers_by_load/0
      ]
    }
  }.

test_is_a_type() ->
  ?assert(node_manager:is_a(router)).

test_get_node_of_type() ->
  ?assertEqual([global:whereis_name(bee_store)], node_manager:get_servers(router)),
  ?assertEqual([], node_manager:get_servers(node)),
  ?assertEqual([], node_manager:get_servers(storage)).

test_sort_servers_by_load() ->
  {ok, Pid1} = node_manager:start_server(bee_store), {ok, Pid2} = node_manager:start_server(bee_store),
  NextAvailable = node_manager:get_next_available(router),
  Routers = node_manager:get_servers(router),
  ?assert(lists:member(NextAvailable, Routers)),
  
  node_manager:get_next_available(node),
  erlang:display(node_manager:get_servers()),
  
  lists:map(fun(P) -> gen_cluster:cast(P, stop) end, [Pid1, Pid2]),
  ok.