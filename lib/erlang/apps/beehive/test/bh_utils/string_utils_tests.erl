-module (string_utils_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

all_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_sub/0,
        fun test_templating_string/0
      ]
    }
  }.

test_sub() ->
  ?assertEqual("Apples", string_utils:sub("apples", "a", "A")),
  ?assertEqual("applEs", string_utils:sub("apples", "e", "E")),
  ?assertEqual("apples", string_utils:sub("apples", "ducks", "fawns")),
  ?assertEqual("apples", string_utils:sub("apples", "lest", "test")),
  ?assertEqual("apeles", string_utils:sub("apples", "app", "ape")),
  passed.


test_templating_string() ->
  ?assertEqual("red apples", string_utils:template_command_string("[[ADJECTIVE]] apples",  [{"[[ADJECTIVE]]", "red"}])),
  ?assertEqual("blue grapes", string_utils:template_command_string("[[ADJECTIVE]] [[NOUN]]",  [{"[[ADJECTIVE]]", "blue"}, {"[[NOUN]]", "grapes"}])),
  ?assertEqual("[[ADJECTIVE]] apples", string_utils:template_command_string("[[ADJECTIVE]] apples",  [{"[[NOUN]]", "pear"}])),
  ?assertEqual("[[ADJECTIVE]] apples", string_utils:template_command_string("[[ADJECTIVE]] apples",  [{"[[NOUN]]", "pear"}, {pickles, "pecans"}])),
  passed.
