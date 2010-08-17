-module (auth_controller_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:dummy_user(),
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
     fun test_post/0,
     fun test_post_missing_email_and_pass/0,
     fun test_post_wrong_email/0,
     fun test_post_wrong_password/0
    ]
   }
  }.

test_post() ->
  {ok, Headers, Body} = post_to_auth([{email, "test@getbeehive.com"},
                                      {password, "test"}]),
  ?assertEqual("HTTP/1.0 200 OK", Headers),
  BodyStruct = mochijson2:decode(lists:last(Body)),
  {struct, Json} = BodyStruct,
  Token = binary_to_list(proplists:get_value(<<"token">>, Json)),
  ?assert(undefined =/= Token),
  passed.

test_post_missing_email_and_pass() ->
  {ok, Headers, Body} = post_to_auth([{email, ""},
                                      {password, ""}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Headers),
  passed.


test_post_wrong_email() ->
  {ok, Headers, Body} = post_to_auth([{email, "noexist@getbeehive.com"},
                                      {password, "test"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Headers),
  passed.

test_post_wrong_password() ->
  {ok, Headers, Body} = post_to_auth([{email, "test@getbeehive.com"},
                                      {password, "wrongpass"}] ),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Headers),
  passed.

post_to_auth(Params) ->
  bh_test_util:fetch_url(post,
                         [{path, "/auth.json"},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]
                        ).

