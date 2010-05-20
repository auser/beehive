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
  restart_by_name/1,
  expand_by_name/1,
  delete/1, new/1, 
  save/1,
  transactional_save/1,
  update_proplist_for_app/2,
  build_on_disk_app_name/1,
  build_app_env/2
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
  case exist(App#app.name) of
    false ->
      case save(App) of
        ok -> 
          ?NOTIFY({app, created, App}),
          {ok, App};
        {'EXIT',{aborted,{no_exists,_}}} -> 
          ?NOTIFY({db, database_not_initialized, app}),
          {error, database_not_initialized};
        E ->
          io:format("Unknown error: ~p~n", [E]),
          {error, did_not_write}
      end;
    true ->
      % ?NOTIFY({app, updated, App}),
      {error, app_exists}
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

expand_by_name(Name) ->
  case find_by_name(Name) of
    [] -> {error, "Cannot find app"};
    App ->
      ?NOTIFY({app, expand, App}),
      {ok, App}
  end.

restart_by_name(Name) ->
  case find_by_name(Name) of
    [] -> {error, "Cannot find app"};
    App ->
      NewApp = App#app{updated_at = date_util:now_to_seconds()},
      ?NOTIFY({app, restart, NewApp}),
      {ok, create(NewApp)}
  end.

update([], _) -> ok;
update(App, NewProps) when is_record(App, app) ->
  NewApp = update_proplist_for_app(App, NewProps),
  ok = save(NewApp),
  {updated, App};
update(Name, NewProps) ->
  App = find_by_name(Name),
  update(App, NewProps).

delete(App) when is_record(App, app) -> db:delete_object(App);
delete(Name) ->
  db:delete_object(#app{name=Name, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(app) ])).

transactional_save(F) ->
  db:transaction(F()).

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
      sticky -> case Val of
        "true" -> {Key, true};
        _ -> {Key, false}
      end;
      timeout -> case Val of
        undefined -> {Key, 10 * 1000};
        _ -> {Key, misc_utils:to_integer(Val)*1000}
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
  
build_on_disk_app_name(App) ->
  App#app.name.

%%-------------------------------------------------------------------
%% @spec (App:app()) ->    {ok, Value}
%% @doc Build environment variables for the application
%%      
%% @end
%%-------------------------------------------------------------------
build_app_env(App, Other) ->
  OtherEnvs = lists:map(fun build_env/1, Other),
  lists:flatten([
    build_env({name, App#app.name}),
    build_env({repos, App#app.url}),
    build_env({sha, App#app.sha}),
    build_env({path, "/usr/bin:/usr/local/bin:/bin"}),
    OtherEnvs
  ]).

build_env({Key, Value}) ->
  RealValue = case Value of
    undefined -> "undefined";
    _ -> Value
  end,
  T = lists:flatten([
    string:to_upper(erlang:atom_to_list(Key)),
    "=",RealValue,""
  ]),
  {env, T}.