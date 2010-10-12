-module (bees_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(bee),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    [{setup,
        fun setup/0,
        fun teardown/1,
        [
          fun create/0,
          fun find_by_name/0,
          fun find_by_id/0,
          fun find_all_grouped_by_host/0,
          fun build_app_env/0,
          fun validate_bee/0,
          fun delete_bee/0
        ]}
    ]
  }.

create() ->
  {ok, Be1} = bees:create(#bee{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app"}),
  % create via proplists
  Props = [{app_name, "another_app"}, {host, {127,0,0,1}}, {port, 8091}],
  {ok, Be2} = bees:create(Props),
  % {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#bee{_='_'}) end),
  Results2 = beehive_db_srv:all(bee),
  ?assert(lists:member(Be2, Results2)),
  ?assert(lists:member(Be1, Results2)),
  passed.

find_by_name() ->
  beehive_db_srv:delete_all(bee),
  {ok, NewBee} = bees:create(#bee{id={"test_app_at_8090_1", {127,0,0,1}, 8090}, app_name = "test_app_at_8090_1"}),
  Bee = bees:find_by_name("test_app_at_8090_1"),
  ?assert(lists:member(NewBee, bees:all())),
  ?assert(Bee#bee.id =:= {"test_app_at_8090_1", {127,0,0,1}, 8090}),
  passed.

find_by_id() ->
  beehive_db_srv:delete_all(bee),
  bees:create(#bee{id={"test_app_at_8090_2", {127,0,0,1}, 8090}, app_name = "test_app_at_8090_2"}),
  Bee = bees:find_by_id({"test_app_at_8090_2", {127,0,0,1}, 8090}),
  ?assert(Bee#bee.id =:= {"test_app_at_8090_2", {127,0,0,1}, 8090}),
  ?assert(1 =:= erlang:length(bees:find_all_by_id({"test_app_at_8090_2", {127,0,0,1}, 8090}))),
  passed.

find_all_grouped_by_host() ->
  beehive_db_srv:delete_all(bee),
  bees:create(#bee{app_name = "test_app", host = {127,0,0,2}}),
  bees:create(#bee{app_name = "another_app", host = {127,0,0,3}}),
  Groups = bees:find_all_grouped_by_host(),
  lists:map(fun({Host, Bees, _Count}) ->
    lists:map(fun(Bee) -> ?assert(Host =:= Bee#bee.host) end, Bees)
  end, Groups),
  passed.

build_app_env() ->
  beehive_db_srv:delete_all(bee),
  {ok, App} = apps:create([{name, "test_app"},
                           {repo_url, bh_test_util:dummy_git_repos_url()}]),
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

validate_bee() ->
  beehive_db_srv:delete_all(bee),
  {ok, Bee} = bees:create(#bee{app_name = "boxcar", host="127.0.0.1", port=9001}),
  ?assert(Bee#bee.id =:= {"boxcar", "127.0.0.1", 9001}),
  passed.

delete_bee() ->
  bh_test_util:setup(bee),
  ?assertMatch([], bees:all()),
  {ok, Bee} = bees:create(#bee{app_name = "boxcar"}),
  ?assertMatch([Bee], bees:all()),
  bees:delete(Bee),
  ?assertMatch([], bees:all()),
  {ok, BeeName} = bees:create(#bee{app_name = "nametest"}),
  ?assertMatch([BeeName], bees:all()),
  bees:delete("nametest"),
  ?assertMatch([], bees:all()),
  passed.
