-module (router_srv_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.

teardown(_S) ->
  ok.

test_info_msg_test_() ->
  {
    setup, 
    fun setup/0, 
    fun teardown/1,
    [
      ?_assert(true)
    ]
  }.
