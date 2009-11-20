%%%-------------------------------------------------------------------
%%% File    : app.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:22:05 PST 2009
%%%-------------------------------------------------------------------

-module (app).

-include ("router.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  find_all_by_name/1,
  all/0,
  exist/1,
  create/1,
  update/1,
  delete/1, new/1
]).

find_by_name(Hostname) ->
  case find_all_by_name(Hostname) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_name(Name) -> 
  db:read({app, Name}).

% Does this app exist?
exist(Name) ->
  case find_by_name(Name) of
    [] -> false;
    _ -> true
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
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  FilteredProplist = validate_app_proplists(FilteredProplist2),
  list_to_tuple([app|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, app)]]).

validate_app_proplists(PropList) ->
  lists:map(fun({Key, Val}) ->
    case Key of
      updated_at -> {Key, date_util:now_to_seconds()};
      _ -> {Key, Val}
    end
  end, PropList).
