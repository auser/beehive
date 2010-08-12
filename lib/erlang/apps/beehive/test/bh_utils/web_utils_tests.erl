-module (web_utils_tests).
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
        fun test_json_parsing/0
      ]
    }
  }.
  
test_json_parsing() ->
  List = [
    {1, "1"},
    {1.4, "1.4"},
    {[1,2], "[1,2]"},
    {"hello", "\"hello\""},
    {true, "true"},
    {false, "false"},
    {{hello, "world"}, "{\"hello\":\"world\"}"},
    {{hello, [1,2,3]}, "{\"hello\":[1,2,3]}"},
    {{app, [{name, "test"}, {var, "10"}]}, "{\"app\":{\"name\":\"test\",\"var\":\"10\"}}"},
    {{all_your, {bases, are}}, "{\"all_your\":{\"bases\":\"are\"}}"}
  ],
  lists:map(fun({Test,EExpected}) ->
    Json = web_utils:to_json(Test),
    Output = erlang:binary_to_list(erlang:iolist_to_binary(Json)),
    ?assertEqual(EExpected, Output)
  end, List),
  passed.
  
