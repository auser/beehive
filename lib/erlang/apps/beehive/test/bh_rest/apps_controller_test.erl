-module (apps_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  bh_test_util:dummy_user(),                    % test@getbeehive.com
  rest_server:start_link(),
  timer:sleep(100),
  ok.

teardown(_X) ->
  beehive_db_srv:delete_all(app),
  ok.

starting_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
     fun get_index/0,
     fun get_index_with_name/0,
     fun get_index_with_wrong_name/0,
     fun post_create_new_app/0,
     fun post_create_new_app_bad_token/0
    ]
   }
  }.

get_index() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/apps.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|_] = bh_test_util:response_json(Response),
  {"apps", Apps} = Json,
  ?assert(is_list(Apps)),
  ?assert(lists:any(fun(E) ->
                        proplists:get_value("name", E) =:= "test_app"
                    end, Apps)),
  passed.

get_index_with_name() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/apps/test_app.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [App|_] = bh_test_util:response_json(Response),
  {"application",[{"name","test_app"}|_]} = App,
  passed.

get_index_with_wrong_name() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/apps/unfound_app.json"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  ?assertMatch([{"error", "App not found"}],
               bh_test_util:response_json(Response)),
  passed.

post_create_new_app() ->
  User = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    perform_post_create([{app_name, "creationtest"},
                        {token, User#user.token}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  ?assertMatch([{"ok","created"}],
                bh_test_util:response_json(Response)),
  passed.

post_create_new_app_bad_token() ->
  {ok, Header, Response} =
    perform_post_create([{app_name, "creationtest"},
                        {token, "badtoken"}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.

perform_post_create(Params) ->
  bh_test_util:fetch_url(post,
                         [{path, "/apps.json"},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]).

