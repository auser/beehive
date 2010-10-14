-module (misc_utils_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_generate_unique_name/0,
        fun propmerge/0,
        fun update_proplist/0
      ]
    }
  }.

test_generate_unique_name() ->
  A = misc_utils:generate_unique_name("butter", 2),
  ?assert(string:left(A, 6) == "butter"),
  ?assert(erlang:length(A) =/= 6),
  passed.

propmerge() ->
  ?assertEqual([{a, "a"}, {b, "b"}, {c, ["c", "not c"]}],
    misc_utils:proplist_merge([{a, "a"}, {b, "b"}, {c, "c"}], [{c, "not c"}])
  ),
  ?assertEqual([{a, "a"}, {b, "b"}, {c, ["c", "not c"]}, {d, "doggie"}],
    misc_utils:proplist_merge([{a, "a"}, {b, "b"}, {c, "c"}], [{c, "not c"}, {d, "doggie"}])
  ),
  passed.

update_proplist() ->
  ?assertEqual([{a, "a"}, {b, "b"}],
               misc_utils:update_proplist([{a, "a"}, {b, "c"}], [{b, "b"}])),
  passed.
