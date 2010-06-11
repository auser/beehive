-module (apps_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "beehive.cfg"]),
  application:set_env(beehive, config_file, ConfigFile),
  
  beehive:start(),
  ok.
  
teardown(_X) ->
  beehive:stop(),
  ok.

starting_test_() ->
  {foreach,
    fun setup/0,
    fun teardown/1,
    [
      fun test_save/0,
      fun test_save_app_with_same_name/0,
      fun test_delete_app/0,
      fun test_read_app/0
    ]
  }.

test_save() ->
  % Delete all
  delete_all(),
  {ok, App1} = apps:save(#app{name="test_app"}),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  [FoundApp1|_Rest] = Results1,
  ?assertEqual(FoundApp1#app.name, App1#app.name),
  % save via proplists
  Props = [{name, "another_app"}, {min_instances, 1}, {max_instances, 10}],
  App2 = apps:new(Props),
  apps:save(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  ?assertEqual([App1,App2], Results2).

test_save_app_with_same_name() ->
  % Delete all
  delete_all(),
  App1 = #app{name="test_app", url="http://github.com/auser/test_app1.git"},
  App2 = #app{name="test_app", url="http://github.com/auser/test_app2.git"},
  {ok, App3} = apps:save(App1),
  {ok, App4} = apps:save(App2),
  ?assert(App3#app.name == "test_app"),
  ?assert(App4#app.name =/= "test_app"),
  passed.

test_delete_app() ->
  delete_all(),
  {ok, App1} = apps:save(#app{name = "test_app"}),
  ?assert(apps:all() == [App1]),
  apps:delete(App1),
  ?assert(apps:all() == []),
  passed.

test_read_app() ->
  delete_all(),
  {ok, App1} = apps:save(#app{name = "test_app"}),
  {ok, FoundApp1} = apps:find_by_name("test_app"),
  ?assertEqual(App1, FoundApp1),
  passed.

% Utils
delete_all() ->
  lists:map(fun(App) ->
    apps:delete(App#app.name)
  end, apps:all()).