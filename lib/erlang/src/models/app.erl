%%%-------------------------------------------------------------------
%%% File    : app.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:22:05 PST 2009
%%%-------------------------------------------------------------------

-module (app).

-include ("beehive.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  find_all_by_name/1,
  all/0,
  exist/1,
  create/1,
  update/1, update_by_name/1,
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
  case exist(App#app.name) of
    true -> ?NOTIFY({app, updated, App});
    false -> ?NOTIFY({app, created, App})
  end,
  db:write(App),
  App;
create(NewProps) ->
  create(new(NewProps)).

update_by_name(Name) ->
  case find_by_name(Name) of
    [] -> {error, "Cannot find app to update"};
    App -> {ok, create(App#app{updated_at = date_util:now_to_seconds()})}
  end.

update(NewProps) ->
  update(new(NewProps)).

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
      name -> case Val of
        undefined -> {Key, generate_unique_name()};
        E -> {Key, E}
      end;
      updated_at -> {Key, date_util:now_to_seconds()};
      _ -> {Key, Val}
    end
  end, PropList).

% Generate a new name for the app, if none are created
generate_unique_name() ->
  NewName = misc_utils:generate_unique_name(5),
  case find_by_name(NewName) of
    [] -> NewName;
    _ -> generate_unique_name()
  end.