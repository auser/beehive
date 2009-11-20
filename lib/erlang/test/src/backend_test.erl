%%%-------------------------------------------------------------------
%%% File    : backend_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Nov 19 23:45:28 PST 2009
%%%-------------------------------------------------------------------

-module (backend_test).
-include ("router.hrl").
-include_lib("eunit/include/eunit.hrl").
-export ([test/0]).

% TESTS
test() -> 
  try
    db:clear_table(backend),
    schema:install(),
    create_test(),
    find_by_name_test(),
    all_test(),
    find_all_grouped_by_host_test(),
    delete_test()
  catch
    throw:Thrown ->
      io:format("Test (~p) failed because ~p~n", [?MODULE, Thrown]),
      throw(Thrown)
  end.

create_test() ->
  db:clear_table(backend),
  schema:install(),
  Be1 = #backend{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app"},
  backend:create(Be1),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#backend{_='_'}) end),
  % Results1 = mnesia:dirty_read({backend, "test_app"}),
  ?assertEqual([Be1], Results1),
  % create via proplists
  Props = [{app_name, "another_app"}, {host, {127,0,0,1}}, {port, 8091}],
  Be2 = backend:new(Props),
  backend:create(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#backend{_='_'}) end),
  ?assertEqual([Be2, Be1], Results2).

find_by_name_test() ->
  Be1 = #backend{app_name = "test_app", id={"test_app", {127,0,0,1}, 8090}},
  Results1 = backend:find_by_name("test_app"),
  ?assertEqual(Be1, Results1).
  
all_test() ->
  All = backend:all(),
  ?assertEqual(2, length(All)).

find_all_grouped_by_host_test() ->
  backend:create(#backend{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app", host={127,0,0,1}}),
  backend:create([{app_name, "another_app"}, {host, {127,0,0,1}}, {port, 8091}]),
  backend:create([{app_name, "yarrrrn pirates"}, {host, {127,0,0,2}}, {port, 8091}]),
  AllBackends = backend:find_all_grouped_by_host(),
  All = lists:map(fun({Host, _Backends, Count}) ->
    {Host, Count}
  end, AllBackends),
  io:format("---- All stuffs: ~p~n", [All]),
  ?assertEqual([
    {{127,0,0,2}, 1},
    {{127,0,0,1}, 2}
  ], All),
  backend:delete("yarrrrn pirates").
  
delete_test() ->
  Be1 = #backend{app_name = "test_app", id={"test_app", {127,0,0,1}, 8090}},
  backend:delete("another_app"),
  ?assertEqual([Be1], backend:all()).