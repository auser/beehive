-module (app_manager_test).
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
        fun instance/0,
        fun add_application/0,
        fun spawn_update_bee_status/0
      ]
    }
  }.

instance()->
  ?assert(undefined =/= app_manager:instance()),
  passed.

add_application() ->
  bh_test_util:delete_all(app),
  User = bh_test_util:dummy_user(),
  O = app_manager:add_application([{name, "bobby-bobbie-o"}], User),
  ?assert(element(1, O) =:= ok),
  passed.

spawn_update_bee_status() ->
  passed.