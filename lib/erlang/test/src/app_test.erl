%%%-------------------------------------------------------------------
%%% File    : app_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Nov 19 23:48:19 PST 2009
%%%-------------------------------------------------------------------

-module (app_test).

-include ("router.hrl").
-include_lib("eunit/include/eunit.hrl").
-export ([test/0]).

% TESTS
test() ->
  try
    db:clear_table(app),
    schema:install(),
    create_test(),
    find_by_name_test(),
    delete_test()
  catch
    throw:Thrown ->
      io:format("Test (~p) failed because ~p~n", [?MODULE, Thrown]),
      throw(Thrown)
  end.

create_test() ->
  App1 = #app{name="test_app"},
  app:create(App1),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  ?assertEqual([App1], Results1),
  % create via proplists
  Props = [{name, "another_app"}, {min_instances, 1}, {max_instances, 10}],
  App2 = app:new(Props),
  app:create(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  ?assertEqual([App1,App2], Results2).

find_by_name_test() ->
  App1 = #app{name="test_app"},
  Results1 = app:find_by_name("test_app"),
  ?assertEqual(App1, Results1).

delete_test() ->
  App1 = #app{name="test_app"},
  app:delete("another_app"),
  ?assertEqual([App1], bee:all()).