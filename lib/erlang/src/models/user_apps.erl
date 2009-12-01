%%%-------------------------------------------------------------------
%%% File    : user_apps.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Nov 30 13:29:11 PST 2009
%%%-------------------------------------------------------------------

-module (user_apps).

-include ("beehive.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_email/1,
  find_by_app_name/1,
  get_owners/1, get_users/1,
  all/0,
  delete/1, delete/2,
  create/1, create/2,
  all_users/1,
  all_apps/1
]).

find_by_email(Email) ->
  case find_all_by_email(Email) of
    [B|_] -> B;
    _ -> []
  end.

all_users(AppName) ->
  UserApps = find_all_by_app_name(AppName),
  lists:map(fun(UserApp) -> UserApp#user_app.user_email end, UserApps).

all_apps(Username) ->
  find_all_by_email(Username).
  
find_by_app_name(Name) ->
  case find_all_by_app_name(Name) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_email(Name) -> 
  db:read({user_app, Name}).
  
find_all_by_app_name(Name) -> 
  case db:match(#user_app{app_name = Name, _='_'}) of
    [U|_] -> U;
    _ -> []
  end.
  
% These aren't going to be incredibly fast
% Get the owners of a specific app
get_owners(App) ->
  UserApps = find_all_by_app_name(App#app.name),
  OwnerApps = lists:filter(fun(UserApp) -> UserApp#user_app.level < ?CONTRIBUTOR_APP_ROLE end, UserApps),
  lists:map(fun(UserApp) -> users:find_by_email(UserApp#user_app.user_email) end, OwnerApps).

% Get the users associated with the app
get_users(App) ->  
  UserApps = find_all_by_app_name(App#app.name),
  lists:map(fun(UserApp) -> users:find_by_email(UserApp#user_app.user_email) end, UserApps).
  
% Insert a new user
create(UsersApp) when is_record(UsersApp, user_app) ->
  db:write(UsersApp),
  UsersApp;

create(NewProps) ->
  create(new(NewProps)).

% Create a user_app
create(User, App) when is_record(User, user), is_record(App, app) ->
  db:write(new([{user_email, User#user.email}, {app_name, App#app.name}]));
create(Email, App) when is_record(App, app) ->
  db:write(new([{user_email, Email}, {app_name, App#app.name}]));
create(Email, AppName) ->
  db:write(new([{user_email, Email}, {app_name, AppName}])).

delete(UsersApp) when is_record(UsersApp, user_app) -> db:delete_object(UsersApp).
delete(Email, AppName) ->
  db:delete_object(#user_app{user_email=Email, app_name=AppName, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(user_app) ])).  

new(NewProps) ->
  PropList = ?rec_info(user_app, #user{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  FilteredProplist = validate_user_app_proplists(FilteredProplist2),
  list_to_tuple([user_app|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, user_app)]]).

validate_user_app_proplists(PropList) ->
  lists:map(fun({Key, Val}) ->
    case Key of
      level -> case Val of
        Int when is_integer(Val) -> {Key, Int};
        undefined -> {Key, ?CONTRIBUTOR_APP_ROLE};
        _Else -> {Key, misc_utils:to_integer(Val)}
      end;
      _ -> {Key, Val}
    end
  end, PropList).
