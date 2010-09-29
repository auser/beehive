-module (http_request_decoder_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "fixtures", "beehive.cfg"]),
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
       fun test_parse_route_from_request/0,
       fun test_split_off_first_subdir/0
      ]
    }
  }.

test_parse_route_from_request() ->
  ?assertEqual(["teeth"], http_request_decoder:parse_route_from_request("teeth.and.nails.test.beehive.com", undefined)),
  ?assertEqual(["hello"], http_request_decoder:parse_route_from_request("hello.test.beehive.com", "test.beehive.com")),
  ?assertEqual(["hello", "world"], http_request_decoder:parse_route_from_request("hello.world.test.beehive.com", "test.beehive.com")),
  ?assertEqual(default, http_request_decoder:parse_route_from_request("test.beehive.com", "test.beehive.com")),
  passed.


test_split_off_first_subdir() ->
  Path = "/rservices/v1/getLocations?g=foo",
  [Subdir| RestOfPath] =
    http_request_decoder:split_off_first_subdirectory(Path),
  ?assertEqual("rservices", Subdir),
  ?assertEqual("/v1/getLocations?g=foo", RestOfPath),
  ShortPath = "/syndication",
  ["syndication"|RestOfShortPath] =
    http_request_decoder:split_off_first_subdirectory(ShortPath),
  ?assertEqual("/", RestOfShortPath),
  passed.
