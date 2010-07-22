-module (beehive_storage_srv_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

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
        fun fetch_or_build_bee_good_test/0,
        fun fetch_or_build_bee_bad_test/0,
        fun can_pull_new_app_test/0
      ]
    }
  }.

% When there is a successful app
fetch_or_build_bee_good_test() ->
  App = bh_test_util:dummy_app(),
  {bee_built, Props} = beehive_storage_srv:fetch_or_build_bee(App),
  ?assertEqual(proplists:get_value(bee_size, Props), 12914),
  % handle_lookup_squashed_repos
  {ok, _Node, Path} = beehive_storage_srv:has_squashed_repos(App, sha_argument_not_used_yet),
  ?assert(filelib:is_file(Path)),
  passed.

fetch_or_build_bee_bad_test() ->
  App = bh_test_util:dummy_app(),
  Out = beehive_storage_srv:fetch_or_build_bee(App#app{name="broken_app", url="svn://no_exists/here/so/fail/please/thank/you"}),
  
  ?assertEqual(error, element(1, Out)),
  ?assertEqual(bee_not_found_after_creation, element(2, Out)),
  
  passed.

can_pull_new_app_test() ->
  % Basically a stub for now
  ?assert(beehive_storage_srv:can_pull_new_app()),
  passed.

