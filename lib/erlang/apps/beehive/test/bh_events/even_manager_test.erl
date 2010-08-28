-module (even_manager_test).
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
        fun startup_t/0
      ]
    }
  }.

startup_t() ->
  erlang:display({running,startup_t,test}),
  passed.