%%%-------------------------------------------------------------------
%%% File    : bee.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 13:15:07 PST 2009
%%%-------------------------------------------------------------------

-module (bee).

% This provides an interface to the bee mnesia table that looks like:
% |----------|
% | bees |
% |----------|
% | name     |
% | host     |
% | ...      |
% |----------|
% 

-include ("router.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  find_all_by_name/1,
  find_all_by_host/1,
  find_all_grouped_by_host/0,
  create/1,
  update/1,
  delete/1, delete/3,
  all/0, new/1
]).

find_by_name(Hostname) ->
  case find_all_by_name(Hostname) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_name(Hostname) -> 
  db:match(#bee{app_name=Hostname, _='_'}).

find_all_by_host(Host) ->
  db:match(#bee{host=Host,_='_'}).
  
find_all_grouped_by_host() ->
  Q = qlc:keysort(#bee.host, db:table(bee)),
  db:transaction(
    fun() -> qlc:fold(fun find_all_grouped_by_host1/2, [], Q) end
  ).

find_all_grouped_by_host1(#bee{host=Host} = B, [{Host, Backends, Sum} | Acc]) ->
  [{Host, [B|Backends], Sum + 1} | Acc];
find_all_grouped_by_host1(#bee{host=Host} = B, Acc) ->
  [{Host, [B], 1}|Acc].

create(Backend) when is_record(Backend, bee) -> 
  case db:write(Backend) of
    ok -> {ok, Backend};
    _ -> {error, did_not_write}
  end;
create(NewProps) ->
  create(new(NewProps)).

% Grrr update!
update(Backend) -> 
  create(Backend).

delete(Backend) when is_record(Backend, bee) -> db:delete_object(Backend);
delete(Name) -> db:delete_object(#bee{app_name=Name, _='_'}).
delete(Name, Host, Port) ->
  db:delete_object(#bee{app_name=Name, host=Host, port=Port, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(bee) ])).

% INERNAL
new(NewProps) ->
  PropList = ?rec_info(bee, #bee{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  Id = make_id_from_proplists(NewProps),
  FilteredProplist3 = validate_bee_proplists(FilteredProplist2),
  FilteredProplist  = [{id, Id}|FilteredProplist3],
  list_to_tuple([bee|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, bee)]]).

% Make an id
make_id_from_proplists(PropList) ->
  Name = proplists:get_value(app_name, PropList, proplists:get_value(name, PropList)),
  Host = proplists:get_value(host, PropList),
  Port = proplists:get_value(port, PropList),
  {Name, Host, Port}.

validate_bee_proplists(PropList) ->
  lists:map(fun({Key, Val}) ->
    case Key of
      port -> {Key, misc_utils:to_integer(Val)};
      _ -> {Key, Val}
    end
  end, PropList).
