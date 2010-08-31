-module (event_manager_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(),
  event_manager:add_handler(dummy_event_handler),
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
  send_message_to_event_manager({hello}),
  timer:sleep(100),
  Messages = dummy_event_handler:get_messages(),
  ?assertEqual([{hello}], Messages),
  passed.

send_message_to_event_manager(Msg) ->
  rpc:cast(node(whereis(node_manager)), node_manager, notify, [Msg]).