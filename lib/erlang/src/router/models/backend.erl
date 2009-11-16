%%%-------------------------------------------------------------------
%%% File    : backend.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 13:15:07 PST 2009
%%%-------------------------------------------------------------------

-module (backend).

% This provides an interface to the backend mnesia table that looks like:
% |----------|
% | backends |
% |----------|
% | name     |
% | host     |
% | ...      |
% |----------|
% 

-include ("router.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  find_all_by_name/1,
  find_all_by_host/1,
  find_all_grouped_by_host/0,
  create/1,
  update/1,
  delete/1,
  all/0
]).

% Test
-export ([run_tests/0]).

find_by_name(Hostname) ->
  case find_all_by_name(Hostname) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_name(Hostname) -> 
  db:match(#backend{app_name=Hostname, _='_'}).

find_all_by_host(Host) ->
  db:match(#backend{host=Host,_='_'}).
  
find_all_grouped_by_host() ->
  Q = qlc:keysort(#backend.host, db:table(backend)),
  db:transaction(
    fun() -> qlc:fold(fun find_all_grouped_by_host1/2, [], Q) end
  ).

find_all_grouped_by_host1(#backend{host=Host} = B, [{Host, Backends, Sum} | Acc]) ->
  [{Host, [B|Backends], Sum + 1} | Acc];
find_all_grouped_by_host1(#backend{host=Host} = B, Acc) ->
  [{Host, [B], 1}|Acc].

create(Backend) when is_record(Backend, backend) -> 
  db:write(Backend);
create(NewProps) ->
  create(new(NewProps)).

% Grrr update!
update(Backend) -> 
  db:write(Backend).

delete(Backend) when is_record(Backend, backend) -> db:delete_object(Backend);
delete(Name) ->
  db:delete_object(#backend{app_name=Name, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(backend) ])).

% INERNAL
new(NewProps) ->
  PropList = ?rec_info(backend, #backend{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  Id = make_id_from_proplists(NewProps),
  FilteredProplist3 = validate_backend_proplists(FilteredProplist2),
  FilteredProplist  = [{id, Id}|FilteredProplist3],
  list_to_tuple([backend|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, backend)]]).

% Make an id
make_id_from_proplists(PropList) ->
  Name = proplists:get_value(app_name, PropList, proplists:get_value(name, PropList)),
  Host = proplists:get_value(host, PropList),
  Port = proplists:get_value(port, PropList),
  {Name, Host, Port}.

validate_backend_proplists(PropList) ->
  lists:map(fun({Key, Val}) ->
    case Key of
      port -> {Key, misc_utils:to_integer(Val)};
      _ -> {Key, Val}
    end
  end, PropList).

% TESTS
run_tests() -> 
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
  create(Be1),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#backend{_='_'}) end),
  % Results1 = mnesia:dirty_read({backend, "test_app"}),
  ?assertEqual([Be1], Results1),
  % create via proplists
  Props = [{app_name, "another_app"}, {host, {127,0,0,1}}, {port, 8091}],
  Be2 = new(Props),
  create(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#backend{_='_'}) end),
  ?assertEqual([Be2, Be1], Results2).

find_by_name_test() ->
  Be1 = #backend{app_name = "test_app", id={"test_app", {127,0,0,1}, 8090}},
  Results1 = find_by_name("test_app"),
  ?assertEqual(Be1, Results1).
  
all_test() ->
  All = all(),
  ?assertEqual(2, length(All)).

find_all_grouped_by_host_test() ->
  create(#backend{id={"test_app", {127,0,0,1}, 8090}, app_name = "test_app", host={127,0,0,1}}),
  create([{app_name, "another_app"}, {host, {127,0,0,1}}, {port, 8091}]),
  create([{app_name, "yarrrrn pirates"}, {host, {127,0,0,2}}, {port, 8091}]),
  AllBackends = find_all_grouped_by_host(),
  All = lists:map(fun({Host, _Backends, Count}) ->
    {Host, Count}
  end, AllBackends),
  io:format("---- All stuffs: ~p~n", [All]),
  ?assertEqual([
    {{127,0,0,2}, 1},
    {{127,0,0,1}, 2}
  ], All),
  delete("yarrrrn pirates").
  
delete_test() ->
  Be1 = #backend{app_name = "test_app", id={"test_app", {127,0,0,1}, 8090}},
  delete("another_app"),
  ?assertEqual([Be1], all()).