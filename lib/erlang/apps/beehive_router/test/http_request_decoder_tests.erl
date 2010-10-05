-module (http_request_decoder_tests).
-include_lib("eunit/include/eunit.hrl").
-import(erlymock).

setup() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "fixtures", "beehive.cfg"]),
  application:set_env(beehive, config_file, ConfigFile),
  ok.

teardown(_X) ->
  application:set_env(beehive, routing_parameter, "Host"),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
       fun test_parse_route_from_request/0,
       fun test_split_off_first_subdir/0,
       fun test_handle_request_based_on_domain/0,
       fun test_handle_request_based_on_path/0
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
  Path = "/service1/v1/endpoint?q=foo",
  [Subdir| RestOfPath] =
    http_request_decoder:split_off_first_subdirectory(Path),
  ?assertEqual("service1", Subdir),
  ?assertEqual("/v1/endpoint?q=foo", RestOfPath),
  ShortPath = "/service2",
  ["service2"|RestOfShortPath] =
    http_request_decoder:split_off_first_subdirectory(ShortPath),
  ?assertEqual("/", RestOfShortPath),
  passed.

test_handle_request_based_on_domain() ->
  erlymock:start(),
  Socket = socket,
  FakeReq = {mochiweb_request,
             undefined,
             'GET',
             "/thepath",
             {1,1},
             {3,
              {"user-agent",{'User-Agent',"curl"},
              {"host",{'Host',"app.mybh.com:8080"},
              {"accept",{'Accept',"*/*"},nil,nil},
              nil},nil}
             }},
  erlymock:stub(beehive_request, new, [Socket], [{return, FakeReq}]),
  erlymock:replay(),
  {ok, Sub, _FwdReq, _Req} = http_request_decoder:handle_request(Socket),
  ?assertEqual(["app"], Sub),
  passed.

test_handle_request_based_on_path() ->
  erlymock:start(),
  application:set_env(beehive, routing_parameter, subdirectory),
  Socket = socket,
  FakeReq = {mochiweb_request,
             undefined,
             'GET',
             "/app/thepath",
             {1,1},
             {3,
              {"user-agent",{'User-Agent',"curl"},
              {"host",{'Host',"mybh.com:8080"},
              {"accept",{'Accept',"*/*"},nil,nil},
              nil},nil}
             }},
  erlymock:stub(beehive_request, new, [Socket], [{return, FakeReq}]),
  erlymock:replay(),
  {ok, Sub, _FwdReq, _Req} = http_request_decoder:handle_request(Socket),
  ?assertEqual("app", Sub),
  passed.
