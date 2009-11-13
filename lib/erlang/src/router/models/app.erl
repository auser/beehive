%%%-------------------------------------------------------------------
%%% File    : app.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:22:05 PST 2009
%%%-------------------------------------------------------------------

-module (app).

-include ("router.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  find_all_by_name/1,
  get/1,
  all/0,
  create/1,
  update/1,
  delete/1
]).

% Test
-export ([test/0]).

find_by_name(Hostname) ->
  case find_all_by_name(Hostname) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_name(Name) -> 
  db:read({app, Name}).

get(Name) ->
  case db:read({app, Name}) of
    [App] -> App;
    [] -> []
  end.
  
% Insert a new app
create(App) when is_record(App, app) ->
  db:write(App);
create(NewProps) ->
  create(new(NewProps)).

update(_) ->
  ok.

delete(App) when is_record(App, app) -> db:delete_object(App);
delete(Name) ->
  db:delete_object(#app{name=Name, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(app) ])).
  
new(NewProps) ->
  PropList = ?rec_info(app, #app{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  list_to_tuple([app|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, app)]]).

  
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
  create(App1),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  ?assertEqual([App1], Results1),
  % create via proplists
  Props = [{name, "another_app"}, {min_instances, 1}, {max_instances, 10}],
  App2 = new(Props),
  create(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#app{_='_'}) end),
  ?assertEqual([App1,App2], Results2).

find_by_name_test() ->
  App1 = #app{name="test_app"},
  Results1 = find_by_name("test_app"),
  ?assertEqual(App1, Results1).

delete_test() ->
  App1 = #app{name="test_app"},
  delete("another_app"),
  ?assertEqual([App1], all()).