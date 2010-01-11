%%%-------------------------------------------------------------------
%%% File    : misc_utils_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Dec 28 12:27:13 PST 2009
%%%-------------------------------------------------------------------

-module (misc_utils_test).

-include_lib("eunit/include/eunit.hrl").

chop_test_() ->
  [
    ?_assertEqual([{hi, "guys"}], misc_utils:chop(["hi guys"])),
    ?_assertEqual([{hi, ""}], misc_utils:chop(["hi"])),
    ?_assertEqual([{hello, "world"}, {how, "areyou"}], misc_utils:chop(["hello world\n how areyou"])),
    ?_assertEqual([{newlines, "are"}, {only, "friendly"}], misc_utils:chop(["newlines are  \n  only friendly"])),
    ?_assertEqual([{path, "/Users/arilerner/Sites/get\ beehive.com"}], misc_utils:chop(["path /Users/arilerner/Sites/get\ beehive.com"])),
    ?_assertEqual([{omg, "what the eff"}, {call, "ghostbusters"}], misc_utils:chop(["omg what the eff\ncall ghostbusters"]))
  ].

propmerge_test_() ->
  [
    ?_assertEqual([{a, "a"}, {b, "b"}, {c, "cnot c"}],
      misc_utils:proplist_merge([{a, "a"}, {b, "b"}, {c, "c"}], [{c, "not c"}])
    ),
    ?_assertEqual([{a, "a"}, {b, "b"}, {c, "cnot c"}, {d, "doggie"}],
      misc_utils:proplist_merge([{a, "a"}, {b, "b"}, {c, "c"}], [{c, "not c"}, {d, "doggie"}])
    )
  ].