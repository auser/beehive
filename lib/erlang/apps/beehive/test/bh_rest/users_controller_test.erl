-module (users_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  bh_test_util:dummy_user(),                    % test@getbeehive.com
  rest_server:start_link(),
  timer:sleep(100),
  ok.

teardown(_X) ->
  beehive_db_srv:delete_all(user),
  beehive_db_srv:delete_all(user_app),
  beehive_db_srv:delete_all(app),
  ok.

starting_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
     fun get_index/0,
     fun get_index_with_email/0,
     fun get_index_with_bad_email/0,
     fun get_user_apps_no_apps/0,
     fun get_user_apps_with_bad_email/0,
     fun get_user_apps_with_an_app/0,
     fun post_new_user/0,
     fun post_new_user_bad_auth/0,
     fun post_new_user_non_admin_auth/0
    ]
   }
  }.

get_index() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|_] = bh_test_util:response_json(Response),
  {"users", Users} = Json,
  ?assert(is_list(Users)),
  ?assert(lists:any(fun(E) ->
                        proplists:get_value("email", E) =:= "test@getbeehive.com"
                    end, Users)),
  passed.

get_index_with_email() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/test@getbeehive.com.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [User|_] = bh_test_util:response_json(Response),
  {"user",[{"email","test@getbeehive.com"},_]} = User,
  passed.

get_index_with_bad_email() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/noexist@nope.com.json"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  passed.

get_user_apps_no_apps() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/test@getbeehive.com/apps.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [User|_] = bh_test_util:response_json(Response),
  {"user",[_,_,{"apps",""}]} = User,
  passed.

get_user_apps_with_bad_email() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/noexist@nope.com/apps.json"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  passed.

get_user_apps_with_an_app() ->
  DummyUser = bh_test_util:dummy_user(),
  App = bh_test_util:dummy_app(),
  {ok, SavedApp} = apps:save(App),
  {ok, _} = user_apps:create(DummyUser, SavedApp),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/test@getbeehive.com/apps.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [User|_] = bh_test_util:response_json(Response),
  {"user",[_,_,FoundApp]} = User,
  ?assertMatch({"apps", [{"name",_}]}, FoundApp),
  passed.

post_new_user() ->
  Admin = bh_test_util:admin_user(),
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "createduser@bhive.com"},
                     {password, "created"},
                     {token, Admin#user.token }
                   ]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [{"user", User}|_] = bh_test_util:response_json(Response),
  ?assertMatch([{"email", "createduser@bhive.com"}], User),
  passed.

post_new_user_bad_auth() ->
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "createduser@bhive.com"},
                     {password, "created"},
                     {token, "unauthed" }
                   ]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  ?assertMatch("Unauthorized.",
               bh_test_util:response_json(Response)),
  passed.

post_new_user_non_admin_auth() ->
  RegUser = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "createduser@bhive.com"},
                     {password, "created"},
                     {token, RegUser#user.token }
                   ]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  ?assertMatch("Unauthorized.",
               bh_test_util:response_json(Response)),
  passed.

perform_post_new(Params) ->
  bh_test_util:fetch_url(post,
                         [{path, "/users.json"},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]).

  
