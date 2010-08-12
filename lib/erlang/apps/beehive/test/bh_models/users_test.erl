-module (users_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

create_hashes_password_test() ->
  {ok, User} = users:create(#user{email = "passtest@getbeehive.com",
                                  password = "atest"}),
  ?assertEqual(User#user.password, bh_md5:hex("atest")),
  passed.

create_new_token_test() ->
  {ok, User} = users:create(#user{email = "token@getbeehive.com",
                                  password = "test"}),
  {ok, UserWithToken} = users:create_new_token_for(User),
  ?assert(undefined =/= UserWithToken#user.token),
  passed.
