-module (app_manager_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
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
  bh_test_util:delete_all(apps),
  OriginalApps = apps:all(),
  User = bh_test_util:dummy_user(),
  O = app_manager:add_application([{name, "bobby-bobbie-o"}], User),
  erlang:display({all_apps, length(apps:all()), length(OriginalApps)}),
  ?assert(element(1, O) =:= ok),
  ?assert(erlang:length(apps:all()) =:= erlang:length(OriginalApps) + 1),
  passed.