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
  case db:write(Backend) of
    ok -> {ok, Backend};
    _ -> {error, did_not_write}
  end;
create(NewProps) ->
  create(new(NewProps)).

% Grrr update!
update(Backend) -> 
  create(Backend).

delete(Backend) when is_record(Backend, backend) -> db:delete_object(Backend);
delete(Name) -> db:delete_object(#backend{app_name=Name, _='_'}).
delete(Name, Host, Port) ->
  db:delete_object(#backend{app_name=Name, host=Host, port=Port, _='_'}).

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
