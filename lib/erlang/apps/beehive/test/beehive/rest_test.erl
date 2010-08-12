-module (rest_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  rest_server:start_link(),
  timer:sleep(100),
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
  {ok, Headers, Body} =
        bh_test_util:fetch_url(get,
                               [{host, "127.0.0.1"},
                                {port, 4999},
                                {path, "/apps.json"},
                                {headers, [{"host", "beehive"}]}]),
  ?assertEqual("HTTP/1.0 200 OK", Headers),
  passed.
