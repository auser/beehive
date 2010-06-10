-module (apps_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  db:clear_table(app),
  db:start(),
  ok.
  
teardown(_X) ->
  db:stop(),
  ok.

starting_test_() ->
  {foreach,
    fun setup/0,
    fun teardown/1,
    [
      fun test_create/0
    ]
  }.

test_create() ->
  erlang:display({apps_test, test_create}),
  App1 = #app{name="test_app"},
  apps:create(App1),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  ?assertEqual([App1], Results1),
  % create via proplists
  Props = [{name, "another_app"}, {min_instances, 1}, {max_instances, 10}],
  App2 = apps:new(Props),
  apps:create(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  erlang:display({result, Results2}),
  ?assertEqual([App1,App2], Results2).