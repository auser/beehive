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
  [Json|Rest] = response_json(Response),
  {"users", Users} = Json,
  ?assert(is_list(Users)),
  ?assert(lists:any(fun(E) -> 
                        proplists:get_value("email", E) =:= "test@getbeehive.com"
                    end, Users)),
  passed.

get_index_with_email() ->
  passed.

response_json(Response) ->
  Json = lists:last(Response),
  BodyStruct = mochijson2:decode(Json),
  parse_json_struct(BodyStruct).

parse_json_struct({struct, List}) ->  parse_json_struct(List);
parse_json_struct({Key, Value}) ->
  {binary_to_list(Key), parse_json_struct(Value) };
parse_json_struct(List) when is_list(List) ->
  lists:map( fun(E) -> 
                 parse_json_struct(E)
             end, List);
parse_json_struct(Binary) when is_binary(Binary) ->  binary_to_list(Binary);
parse_json_struct(Int) when is_integer(Int) -> Int.

