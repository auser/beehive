-module (rest_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  application:start(beehive_router),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_connectable/0
      ]
    }
  }.

test_connectable() ->
  gen_tcp:connect({0,0,0,0}, 8080, [binary]),
  receive
    X -> 
      erlang:display({got, X})
  after 1000 ->
    erlang:display({hrm, no_connect})
  end,
  passed.