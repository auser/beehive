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
  create/1,
  update/1,
  delete/1,
  all/0
]).

-export ([test/0]).

find_by_name(Hostname) ->
  case find_all_by_name(Hostname) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_name(Hostname) -> db:read({backend, Hostname}).

create(Backend) when is_record(Backend, backend) -> db:write(Backend);
create(NewProps) -> db:write(new(NewProps)).

update(Backend) -> create(Backend).

delete(Key) ->
  db:delete(backend, Key).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(backend) ])).

% INERNAL
new(NewProps) ->
  PropList = ?rec_info(backend, #backend{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist = new_or_previous_value(FilteredProplist1, PropList, []),
  list_to_tuple([backend|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, backend)]]).

new_or_previous_value(_NewProplist, [], Acc) -> Acc;
new_or_previous_value(NewProplist, [{K,V}|Rest], Acc) ->
  case proplists:is_defined(K,NewProplist) of
    true -> 
      NewV = proplists:get_value(K, NewProplist),
      new_or_previous_value(NewProplist, Rest, [{K, NewV}|Acc]);
    false ->
      new_or_previous_value(NewProplist, Rest, [{K, V}|Acc])
  end.


% TESTS
test() ->
  try
    mnesia:clear_table(backend),
    schema:install(),
    create_test(),
    find_by_name_test(),
    all_test(),
    delete_test()
  catch
    throw:Thrown ->
      io:format("Test (~p) failed because ~p~n", [?MODULE, Thrown]),
      throw(Thrown)
  end.

create_test() ->
  Be1 = #backend{app_name = "test_app"},
  create(Be1),
  {atomic,Results1} = mnesia:transaction(fun() -> mnesia:match_object(#backend{_='_'}) end),
  ?assertEqual([Be1], Results1),
  % create via proplists
  Props = [{app_name, "another_app"}],
  Be2 = new(Props),
  create(Props),
  {atomic,Results2} = mnesia:transaction(fun() -> mnesia:match_object(#backend{_='_'}) end),
  ?assertEqual([Be1, Be2], Results2).

find_by_name_test() ->
  Be1 = #backend{app_name = "test_app"},
  Results1 = find_by_name("test_app"),
  ?assertEqual(Be1, Results1).
  
all_test() ->
  Be1 = #backend{app_name = "test_app"},
  Be2 = #backend{app_name = "another_app"},
  ?assertEqual([Be1, Be2], all()).
  
delete_test() ->
  Be1 = #backend{app_name = "test_app"},
  delete("another_app"),
  ?assertEqual([Be1], all()).