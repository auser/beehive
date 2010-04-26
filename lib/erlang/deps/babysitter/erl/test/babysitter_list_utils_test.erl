-module (babysitter_list_utils_test).
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
        fun test_merge_proplists/0
      ]
    }
  }.

test_merge_proplists() ->
  PList = [{},[{bundle,{command,"echo \"Bundle java stuff\""}},{bundle,{before,"echo \"Before bundle\""}},{bundle,{aft,"echo \"After bundle\""}},{mount,{command,"echo \"mounting\""}}],{}],
  Out = babysitter_list_utils:merge_proplists(PList),
  Match = [{mount,[{command,"echo \"mounting\""}]}, {bundle,[{command,"echo \"Bundle java stuff\""},{before,"echo \"Before bundle\""},{aft,"echo \"After bundle\""}]}],
  ?assertEqual(Out, Match).