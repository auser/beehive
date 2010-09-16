-module (apps_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(bee),
  timer:sleep(100),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {foreach,
    fun setup/0,
    fun teardown/1,
    [
      fun test_save/0,
      fun test_save_app_with_same_name/0,
      fun test_save_app_with_upper_case_name/0,
      fun test_delete_app/0,
      fun test_branch/0,
      fun test_read_app/0
    ]
  }.

test_save() ->
  % Delete all
  Table = app,
  bh_test_util:delete_all(Table),
  {ok, App1} = apps:save(#app{name="test_app"}),
  ?assert(App1#app.branch =:= "master"),
  % Hardcode search in ets
  Results1 = lists:map(
    fun({_, R}) -> R end, 
    ets:select(Table, [{{'_', #app{_='_'}}, [], ['$_']}])
  ),
  [FoundApp1|_Rest] = Results1,
  ?assertEqual(FoundApp1#app.name, App1#app.name),
  % save via proplists
  Props = [{name, "another_app"}, {min_instances, 1}, {max_instances, 10}],
  App2 = apps:new(Props),
  apps:save(Props),
  % Another hardcoded search in ets
  Results2 = lists:map(
    fun({_, R}) -> R#app.name end, 
    ets:select(Table, [{{'_', #app{_='_'}}, [], ['$_']}])
  ),
  ?assertEqual([App1#app.name,App2#app.name], lists:reverse(Results2)).

test_save_app_with_same_name() ->
  % Delete all
  bh_test_util:delete_all(app),
  lists:map(fun(App) -> apps:delete(App) end, apps:all()),
  {ok, App3} = apps:create(#app{name="test_app", repo_url="http://github.com/auser/test_app1.git"}),
  {ok, App4} = apps:create(#app{name="test_app", repo_url="http://github.com/auser/test_app2.git"}),
  ?assert(App3#app.name == "test_app"),
  ?assert(App4#app.name =/= "test_app"),
  passed.
  
test_save_app_with_upper_case_name() ->
  bh_test_util:delete_all(app),
  {ok, App} = apps:create(#app{name="TestApp", repo_url="http://github.com/auser/test_app2.git"}),
  ?assertEqual("testapp", App#app.name),
  passed.

test_branch() ->
  lists:map(fun(App) -> apps:delete(App) end, apps:all()),
  {ok, App1} = apps:create(#app{name="test_app", repo_url="http://github.com/auser/test_app1.git"}),
  ?assert(App1#app.branch == "master"),
  {ok, App2} = apps:create(#app{name="test_app/other_branch", repo_url="http://github.com/auser/test_app1.git"}),
  ?assert(App2#app.branch == "other_branch"),
  passed.

test_delete_app() ->
  lists:map(fun(App) -> apps:delete(App) end, apps:all()),
  {ok, App1} = apps:save(#app{name = "test_app"}),
  ?assert(apps:all() == [App1]),
  apps:delete(App1),
  ?assert(apps:all() == []),
  passed.

test_read_app() ->
  bh_test_util:delete_all(app),
  {ok, App1} = apps:save(#app{name = "test_app"}),
  FoundApp1 = apps:find_by_name("test_app"),
  ?assertEqual(App1#app.name, FoundApp1#app.name),
  passed.

