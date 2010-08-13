-module (bees_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  bh_test_util:dummy_user(),                    % test@getbeehive.com
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
     fun get_index/0,
     fun post_create_bee/0
    ]
   }
  }.

get_index() ->
  {ok, _Bee} = bees:create(#bee{app_name = "boxcar", 
                                host="127.0.0.1", port=9001}),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/bees.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|_] = bh_test_util:response_json(Response),
  {"bees", Bees} = Json,
  ?assert(is_list(Bees)),
  ?assert(lists:any(fun(E) ->
                        proplists:get_value("app_name", E) =:= "boxcar"
                    end, Bees)),
  passed.

post_create_bee() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(post,
                           [{path, "/bees.json"},
                            {headers, [{"Content-Type",
                                        "application/x-www-form-urlencoded" }]},
                            {params, [{app_name, "beetest"},
                                      {host,     "localhost"},
                                      {port,     "10000"}
                                     ]}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  ?assertMatch([{"message", "Added bee beetest"}],
               bh_test_util:response_json(Response)),
  passed.
