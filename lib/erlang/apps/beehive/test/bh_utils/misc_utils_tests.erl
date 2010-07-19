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
        fun chop_test/0,
        fun propmerge_test/0,
        fun update_proplist_test/0
      ]
    }
  }.

test_generate_unique_name() ->
  A = misc_utils:generate_unique_name("butter", 2),
  ?assert(string:left(A, 6) == "butter"),
  ?assert(erlang:length(A) =/= 6),
  passed.

chop_test() ->
  ?assertEqual([{hi, "guys"}], misc_utils:chop(["hi guys"])),
  ?assertEqual([{hi, ""}], misc_utils:chop(["hi"])),
  ?assertEqual([{hello, "world"}, {how, "areyou"}], misc_utils:chop(["hello world\n how areyou"])),
  ?assertEqual([{newlines, "are"}, {only, "friendly"}], misc_utils:chop(["newlines are  \n  only friendly"])),
  ?assertEqual([{path, "/Users/arilerner/Sites/get\ beehive.com"}], misc_utils:chop(["path /Users/arilerner/Sites/get beehive.com"])),
  ?assertEqual([{omg, "what the eff"}, {call, "ghostbusters"}], misc_utils:chop(["omg what the eff\ncall ghostbusters"])),
  passed.

propmerge_test() ->
  ?assertEqual([{a, "a"}, {b, "b"}, {c, ["c", "not c"]}],
    misc_utils:proplist_merge([{a, "a"}, {b, "b"}, {c, "c"}], [{c, "not c"}])
  ),
  ?assertEqual([{a, "a"}, {b, "b"}, {c, ["c", "not c"]}, {d, "doggie"}],
    misc_utils:proplist_merge([{a, "a"}, {b, "b"}, {c, "c"}], [{c, "not c"}, {d, "doggie"}])
  ),
  passed.

update_proplist_test() ->
  ?assertEqual([{a, "a"}, {b, "b"}], misc_utils:update_proplist([{a, "a"}, {b, "c"}], [{b, "b"}])),
  passed.