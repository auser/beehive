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
          save/1,
          all_users/1,
          all_apps/1
         ]).

-define (DB, beehive_db_srv).

-export ([
          validate_user_app/1,
          to_proplist/1
         ]).

find_by_email(Email) ->
  case find_all_by_email(Email) of
    [B|_] -> B;
    _ -> not_found
  end.

all_users(AppName) ->
  UserApps = find_all_by_app_name(AppName),
  lists:map(fun(UserApp) ->
                UserEmail = UserApp#user_app.user_email,
                users:find_by_email(UserEmail)
            end, UserApps).

%% @doc Finds user's apps.  Ensures that apps exist. Takes email as parameter.
all_apps(Email) ->
  UserApps = find_all_by_email(Email),
  TheApps = lists:map(fun(UserApp) ->
                          Appname = UserApp#user_app.app_name,
                          apps:find_by_name(Appname)
                      end, UserApps),
  lists:filter(fun(App) ->
                   case App of
                     X when is_record(X, app) ->
                       true;
                     _ -> false
                   end
               end, TheApps).

find_by_app_name(Name) ->
  case find_all_by_app_name(Name) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_email(Email) ->
  case ?DB:match(#user_app{user_email = Email, _='_'}) of
    Apps when is_list(Apps) -> Apps;
    App  when is_record(App, user_app) -> [App];
    _ -> []
  end.

find_all_by_app_name(Name) ->
  ?DB:match(#user_app{app_name = Name, _='_'}).

find_user_app(UserEmail, AppName) ->
  ?DB:match(#user_app{app_name = AppName, user_email = UserEmail, _='_'}).

%% These aren't going to be incredibly fast
%% Get the owners of a specific app
get_owners(App) ->
  UserApps = find_all_by_app_name(App#app.name),
  OwnerApps = lists:filter(fun(UserApp) -> UserApp#user_app.level < ?CONTRIBUTOR_APP_ROLE end, UserApps),
  lists:map(fun(UserApp) -> users:find_by_email(UserApp#user_app.user_email) end, OwnerApps).

%% Get the users associated with the app
get_users(App) ->
  UserApps = find_all_by_app_name(App#app.name),
  lists:map(fun(UserApp) -> users:find_by_email(UserApp#user_app.user_email) end, UserApps).

%% Insert a new user
create(UsersApp) when is_record(UsersApp, user_app) ->
  case catch ?DB:write(user_app, UsersApp#user_app.app_name, validate_user_app(UsersApp)) of
    ok -> {ok, UsersApp};
    Else -> {error, Else}
  end;

create(NewProps) -> save(validate_user_app(new(NewProps))).

%% Create a user_app
create(User, App) when is_record(User, user) andalso is_record(App, app) ->
  create([{user_email, User#user.email}, {app_name, App#app.name}]);
create(Email, App) when is_record(App, app) ->
  create([{user_email, Email}, {app_name, App#app.name}]);
create(Email, AppName) ->
  create([{user_email, Email}, {app_name, AppName}]).


%% Save the user_app
save(UserApp) when is_record(UserApp, user_app) ->
  case catch ?DB:write(user_app, UserApp#user_app.app_name, UserApp) of
    ok -> {ok, UserApp};
    {'EXIT',{aborted,{no_exists,_}}} ->
      ?NOTIFY({db, database_not_initialized, user_app}),
      {error, database_not_initialized};
    _E ->
      %% TODO: Investigate why this EVER happens...
      {error, did_not_write}
  end;
save([]) -> invalid;
save(Proplists) when is_list(Proplists) ->
  case from_proplists(Proplists) of
    {error, _} = T -> T;
    Bee ->
      save(Bee)
  end;
save(Func) when is_function(Func) ->
  ?DB:save(Func);

save(Else) -> {error, {cannot_save, Else}}.


new([]) -> error;
new(UsersApp) when is_record(UsersApp, user_app) -> validate_user_app(UsersApp);
new(Proplist) when is_list(Proplist) -> validate_user_app(from_proplists(Proplist));
new(Else) -> {error, {cannot_make_user_app, Else}}.

delete(UsersApp) when is_record(UsersApp, user_app) -> ?DB:delete(user_app, UsersApp);
delete([]) -> invalid;
delete(Else) -> {error, {cannot_delete, Else}}.

delete(Email, AppName) ->
  UserApp = find_user_app(Email, AppName),
  ?DB:delete(UserApp).

all() -> ?DB:all(user_app).

from_proplists(Proplists) -> from_proplists(Proplists, #user_app{}).
from_proplists([], UserApp)  -> UserApp;
from_proplists([{user_email, V}|Rest], UserApp) ->
  from_proplists(Rest, UserApp#user_app{user_email = V});
from_proplists([{app_name, V}|Rest], UserApp) ->
  from_proplists(Rest, UserApp#user_app{app_name = V});
from_proplists([{level, V}|Rest], UserApp) ->
  from_proplists(Rest, UserApp#user_app{level = V});
from_proplists([_Else|Rest], UserApp) ->
  from_proplists(Rest, UserApp).

to_proplist(UserApp) -> to_proplist(record_info(fields, user_app), UserApp, []).
to_proplist([], _UserApp, Acc) -> Acc;
to_proplist([user_email|Rest], #user_app{user_email = Id} = UserApp, Acc) ->
  to_proplist(Rest, UserApp, [{user_email, Id}|Acc]);
to_proplist([app_name|Rest], #user_app{app_name = Name} = UserApp, Acc) ->
  to_proplist(Rest, UserApp, [{app_name, Name}|Acc]);
to_proplist([level|Rest], #user_app{level=Level} = UserApp, Acc) ->
  to_proplist(Rest, UserApp, [{level, Level}|Acc]);
to_proplist([_Else|Rest], UserApp, Acc) -> to_proplist(Rest, UserApp, Acc).

validate_user_app(UserApp) when is_record(UserApp, user_app) -> validate_user_app(record_info(fields, user_app), UserApp);
validate_user_app(Else) -> Else.

validate_user_app([], UserApp) ->  UserApp;
%% Validate the user_email
validate_user_app([user_email|_Rest], #user_app{user_email = undefined} = _UserApp) -> {error, no_user_email_given};
validate_user_app([user_email|Rest], #user_app{user_email = Email} = UserApp) ->
  case users:find_by_email(Email) of
    [] -> {error, user_not_found};
    _User -> validate_user_app(Rest, UserApp)
  end;
%% Validate the app
validate_user_app([app_name|_Rest], #user_app{app_name = undefined}) -> 
  {error, no_app_name_given};
validate_user_app([app_name|Rest], #user_app{app_name = AppName} = UserApp) ->
  case apps:find_by_name(AppName) of
    [] -> {error, app_not_found};
    _App -> validate_user_app(Rest, UserApp)
  end;
%% Validate the level
validate_user_app([level|Rest], #user_app{level = undefined} = UserApp) -> 
  validate_user_app(Rest, UserApp#user_app{level = ?REGULAR_USER_LEVEL});

%% Validate others?
validate_user_app([_H|Rest], UA) -> validate_user_app(Rest, UA).
