-module (babysitter_parser_test).
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
        fun test_simple_parsing/0,
        fun test_file_parsing/0
      ]
    }
  }.

test_simple_parsing() ->
  Matches = [
    ["bundle: hello\n", [{bundle, [{command, "hello"}]}]],
    ["bundle: 'hello'\n", [{bundle, [{command, "'hello'"}]}]],
    ["bundle: \"hello\"\n", [{bundle, [{command, "\"hello\""}]}]],
    ["bundle.before: \"world\"\n", [{bundle, [{pre, "\"world\""}]}]],
    ["bundle.after: lickity\n", [{bundle, [{post, "lickity"}]}]],
    ["mount: echo 'hello world' | awk 'BEGIN {print $2} END'\n", [{mount, [{command, "echo 'hello world' | awk 'BEGIN {print $2} END'"}]}]],
    ["bundle: do echo 'hello'\n echo 'bob'end", [{bundle, [{command, " echo 'hello'\n echo 'bob'"}]}]]
  ],
  lists:map(fun([H|T]) ->
    ?assertEqual(hd(T), babysitter_config_parser:parse(H))
  end, Matches).

test_file_parsing() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  File = filename:join([Dir, "test", "config.conf"]),
  X = babysitter_config_parser:file(File),
  Match = [{mount,[{command,"\n  echo \"mounting\"\n"}]},{bundle,[{command,"echo \"Bundle java stuff\""},{pre,"echo \"Before bundle\""},{post,"echo \"After bundle\""}]}],
  ?assertEqual(Match, X).