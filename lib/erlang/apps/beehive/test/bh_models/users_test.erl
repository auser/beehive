-module (users_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    [{setup,
        fun setup/0,
        fun teardown/1,
        [
         fun create_hashes_password/0,
         fun create_new_token/0
        ]}
    ]
  }.


create_hashes_password() ->
    {ok, User} = users:create(#user{email = "passtest@getbeehive.com",
                                    password = "atest"}),
    ?assertEqual(User#user.password, bh_md5:hex("atest")),
    passed.

create_new_token() ->
    {ok, User} = users:create(#user{email = "token@getbeehive.com",
                                    password = "test"}),
    {ok, UserWithToken} = users:create_new_token_for(User),
    ?assert(undefined =/= UserWithToken#user.token),
    passed.
