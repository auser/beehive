-module (app_manager_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(),
  ok.
  
teardown(_X) ->
  bh_test_util:teardown(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun instance_test/0,
        fun add_application_test/0
      ]
    }
  }.

instance_test()->
  ?assert(undefined =/= app_manager:instance()),
  passed.

add_application_test() ->
  erlang:display({add_application_test}),
  Apps = apps:all(),
  erlang:display({apps, Apps}),
  app_manager:add_application([{name, "bobby-bobbie-o"}]),
  passed.