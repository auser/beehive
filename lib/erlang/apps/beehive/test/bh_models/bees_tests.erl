-module (bees_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  test_util:setup_test(bee),
  ok.
  
teardown(_X) ->
  test_util:teardown_test(),
  ok.

starting_test_() ->
  {"Bees test",
    [{setup,
        fun setup/0,
        fun teardown/1,
        [
          fun create_test/0,
          fun find_by_name_test/0,
          fun find_by_id_test/0,
          fun find_all_grouped_by_host_test/0,
          fun build_app_env_test/0,
          fun validate_bee_test/0
        ]}
    ]
  }.

create_test() ->
  beehive_db_srv:delete_all(bee),
  {ok, Be1} = bees:create(#bee{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app"}),
  % create via proplists
  Props = [{app_name, "another_app"}, {host, {127,0,0,1}}, {port, 8091}],
  {ok, Be2} = bees:create(Props),
  % {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#bee{_='_'}) end),
  Results2 = beehive_db_srv:all(bee),
  ?assertEqual([Be2, Be1], Results2),
  passed.

find_by_name_test() ->
  beehive_db_srv:delete_all(bee),
  bees:create(#bee{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app"}),
  Bee = bees:find_by_name("test_app"),
  ?assert(Bee#bee.id =:= {"test_app", {127,0,0,1}, 8090}),
  passed.

find_by_id_test() ->
  beehive_db_srv:delete_all(bee),
  bees:create(#bee{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app"}),
  Bee = bees:find_by_id({"test_app", {127,0,0,1}, 8090}),
  ?assert(Bee#bee.id =:= {"test_app", {127,0,0,1}, 8090}),
  ?assert(1 =:= erlang:length(bees:find_all_by_id({"test_app", {127,0,0,1}, 8090}))),
  passed.

find_all_grouped_by_host_test() ->
  beehive_db_srv:delete_all(bee),
  bees:create(#bee{app_name = "test_app", host = {127,0,0,2}}),
  bees:create(#bee{app_name = "another_app", host = {127,0,0,3}}),
  Groups = bees:find_all_grouped_by_host(),
  lists:map(fun({Host, Bees, _Count}) ->
    lists:map(fun(Bee) -> ?assert(Host =:= Bee#bee.host) end, Bees)
  end, Groups),
  passed.

build_app_env_test() ->
  beehive_db_srv:delete_all(bee),
  {ok, App} = apps:create([{name, "test_app"}]),
  {ok, Bee} = bees:create(#bee{app_name = "test_app", host = "127.0.0.2"}),
  {ok, _App, Env} = bees:build_app_env(Bee, App),
  lists:map(fun({Key, Value}) ->
    case Key of
      env ->
        % Weak test, but gets the job done
        Strings = string:tokens(Value, "="),
        ?assertEqual(2, erlang:length(Strings)),
        ok;
      _ -> ok
    end
  end, Env),
  passed.

validate_bee_test() ->
  beehive_db_srv:delete_all(bee),
  {ok, Bee} = bees:create(#bee{app_name = "boxcar", host="127.0.0.1", port=9001}),
  ?assert(Bee#bee.id =:= {"boxcar", "127.0.0.1", 9001}),
  passed.