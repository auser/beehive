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
        fun build_bee_good/0,
        fun build_bee_bad/0,
        fun can_pull_new_app/0
      ]
    }
  }.

% When there is a successful app
build_bee_good() ->
  App = bh_test_util:dummy_app(),
  {bee_built, Props} = beehive_storage_srv:build_bee(App),
  ?assertEqual("812f9cf168719b4ff9c84a9817b05b2e6cfe7297", proplists:get_value(sha, Props)),
  % handle_lookup_squashed_repos
  {ok, _Node, Path} = beehive_storage_srv:has_squashed_repos(App, sha_argument_not_used_yet),
  ?assert(filelib:is_file(Path)),
  passed.

build_bee_bad() ->
  App = bh_test_util:dummy_app(),
  Out = beehive_storage_srv:build_bee(App#app{url="svn://no_exists/here/so/fail/please/thank/you"}),
  {error, {babysitter, NewApp}} = Out,
  ?assert(NewApp#app.latest_error =/= undefined),
  passed.

can_pull_new_app() ->
  % Basically a stub for now
  ?assert(beehive_storage_srv:can_pull_new_app()),
  passed.

