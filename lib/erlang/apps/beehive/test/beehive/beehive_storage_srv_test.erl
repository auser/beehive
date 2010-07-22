-module (beehive_storage_srv_test).
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
        fun fetch_or_build_bee_test/0,
        fun can_pull_new_app_test/0
      ]
    }
  }.

fetch_or_build_bee_test() ->
  App = bh_test_util:dummy_app(),
  
  {bee_built, Props} = beehive_storage_srv:fetch_or_build_bee(App),
  ?assertEqual(proplists:get_value(bee_size, Props), 12914),
  passed.

can_pull_new_app_test() ->
  % Basically a stub for now
  ?assert(beehive_storage_srv:can_pull_new_app()),
  passed.

