-module (beehive_git_srv_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  % beehive_git_srv:start_link(),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun start_up_t/0
      ]
    }
  }.

start_up_t() ->
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 9148, [binary]),
  erlang:display({socket, Sock}),
  gen_tcp:close(Sock),
  passed.