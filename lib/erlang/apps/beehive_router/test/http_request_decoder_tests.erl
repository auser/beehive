-module (http_request_decoder_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "beehive.cfg"]),
  application:set_env(beehive, config_file, ConfigFile),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_parse_route_from_request/0
      ]
    }
  }.
  
test_parse_route_from_request() ->
  ?assertEqual(["teeth"], http_request_decoder:parse_route_from_request("teeth.and.nails.test.beehive.com", undefined)),
  ?assertEqual(["hello"], http_request_decoder:parse_route_from_request("hello.test.beehive.com", "test.beehive.com")),
  ?assertEqual(["hello", "world"], http_request_decoder:parse_route_from_request("hello.world.test.beehive.com", "test.beehive.com")),
  ?assertEqual(base, http_request_decoder:parse_route_from_request("test.beehive.com", "test.beehive.com")),
  passed.