%%%-------------------------------------------------------------------
%%% File    : apps.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:22:05 PST 2009
%%%-------------------------------------------------------------------

-module (apps).

-include ("beehive.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  find_all_by_name/1,
  all/0,
  exist/1,
  create/1,
  update/2, 
  update_by_name/1,
  delete/1, new/1, 
  update_proplist_for_app/2,
  build_on_disk_app_name/1
]).

% Find the first application named Name
find_by_name(Name) ->
  case find_all_by_name(Name) of
    [B|_] -> B;
    _ -> []
  end.

% Find the apps that are named Name
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

  case db:write(App) of
    ok -> 
      case exist(App#app.name) of
        true -> ?NOTIFY({app, updated, App});
        false -> ?NOTIFY({app, created, App})
      end,
      {ok, App};
    {'EXIT',{aborted,{no_exists,_}}} -> 
      ?NOTIFY({db, database_not_initialized, app}),
      {error, database_not_initialized};
    E ->
      io:format("Unknown error: ~p~n", [E]),
      {error, did_not_write}
  end;
create(NewProps) ->
  create(new(NewProps)).

update_by_name(Name) ->
  case find_by_name(Name) of
    [] -> {error, "Cannot find app to update"};
    App -> 
      NewApp = App#app{updated_at = date_util:now_to_seconds()},
      ?NOTIFY({app, updated, NewApp}),
      {ok, create(NewApp)}
  end.

update([], _) -> ok;
update(App, NewProps) when is_record(App, app) ->
  NewApp = update_proplist_for_app(App, NewProps),
  save(NewApp);
update(Name, NewProps) ->
  App = find_by_name(Name),
  update(App, NewProps).

delete(App) when is_record(App, app) -> db:delete_object(App);
delete(Name) ->
  db:delete_object(#app{name=Name, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(app) ])).

save(App) when is_record(App, app) ->
  db:write(App).
  
new(NewProps) ->
  PropList = ?rec_info(app, #app{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  FilteredProplist = validate_app_proplists(FilteredProplist2),
  list_to_tuple([app|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, app)]]).

update_proplist_for_app(App, NewProps) ->
  PropList = ?rec_info(app, App),
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
      type -> {Key, misc_utils:to_atom(Val)};
      bee_picker -> {Key, misc_utils:to_atom(Val)};
      routing_param -> {Key, misc_utils:to_atom(Val)};
      min_instances -> {Key, misc_utils:to_integer(Val)};
      max_instances -> {Key, misc_utils:to_integer(Val)};
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
  
build_on_disk_app_name(App) ->
  lists:flatten([
    lists:append([App#app.name, misc_utils:to_list(App#app.updated_at)])
  ]).