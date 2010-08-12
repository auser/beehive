-module (users_controller_test).
-include_lib("eunit/include/eunit.hrl").

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
     fun get_index_with_email/0
    ]
   }
  }.

get_index() ->
  {ok, Header, Response} = 
    bh_test_util:fetch_url(get,
                           [{path, "/users.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|Rest] = bh_test_util:response_json(Response),
  {"users", Users} = Json,
  ?assert(is_list(Users)),
  ?assert(lists:any(fun(E) -> 
                        proplists:get_value("email", E) =:= "test@getbeehive.com"
                    end, Users)),
  passed.

get_index_with_email() ->
  passed.
