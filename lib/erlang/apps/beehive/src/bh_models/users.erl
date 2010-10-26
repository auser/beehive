%%%-------------------------------------------------------------------
%%% File    : users.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Sat Nov 28 21:28:31 PST 2009
%%%-------------------------------------------------------------------

-module (users).

-include ("beehive.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  initialize/0,
  find_by_email/1,
  find_all_by_email/1,
  all/0,
  exist/1,
  create/1,
  save/1,
  update/1,
  delete/1,
  create_new_token_for/1, create_new_token_for/2,
  find_by_token/1,
  is_user_token/2,
  new/1
]).

-define (DB, beehive_db_srv).

find_by_email(Hostemail) ->
  case find_all_by_email(Hostemail) of
    [B|_] -> B;
    _ -> not_found
  end.

find_all_by_email(Name) ->
  case ?DB:read(user, Name) of
    Users when is_list(Users) -> Users;
    User  when is_record(User, user) -> [User];

    _ -> []
  end.

find_by_token(Token) ->
  case ?DB:match(#user{token = Token, _='_'}) of
    [U|_] -> U;
    _ -> not_found
  end.

%% Does this user exist?
exist(Name) ->
  case find_by_email(Name) of
    not_found -> false;
    _ -> true
  end.

%% New
new([]) -> error;
new(User) when is_record(User, user) -> User;
new(Proplist) when is_list(Proplist) -> from_proplists(Proplist);
new(Else) -> {error, {cannot_make_new_user, Else}}.

%% Save the user_app
save(User) when is_record(User, user) ->
  RealUser = validate_user(User),
  case catch ?DB:write(user, RealUser#user.email, RealUser) of
    ok -> {ok, RealUser};
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
    User -> save(User)
  end;
save(Func) when is_function(Func) -> ?DB:save(Func);
save(Else) -> {error, {cannot_save, Else}}.

%% Insert a new user
create(Given) ->
  User = new(Given),
  EventMsg = case exist(User#user.email) of
    true -> {user, updated, User};
    false -> {user, created, User}
  end,
  case save(User#user{password = bh_md5:hex(User#user.password)}) of
    {ok, _} = T ->
      ?NOTIFY(EventMsg),
      T;
    Else -> Else
  end.

update(NewProps) ->
  create(new(NewProps)).

delete(User) when is_record(User, user) -> ?DB:delete(User);
delete(Name) ->
  ?DB:delete(#user{email=Name, _='_'}).

all() ->
  ?DB:all(user).

create_new_token_for(User) when is_record(User, user) ->
  NewToken = bh_md5:hex(lists:flatten([
    User#user.email,
    misc_utils:to_list(date_util:now_to_seconds())
  ])),
  save(User#user{token = NewToken}).

is_user_token(Email, Token) ->
  case find_by_email(Email) of
    [] -> false;
    User ->
      User#user.token =:= Token
  end.


%% Create a new token for the user
create_new_token_for(Email, Password) ->
  case find_by_email(Email) of
    User when is_record(User, user) ->
      case User#user.password =:= bh_md5:hex(Password) of
        false -> invalid_password;
        true ->
          create_new_token_for(User)
      end;
    _ -> {error, not_found}
  end.


initialize() ->
  spawn(fun() ->
    timer:sleep(200),
    add_root_user()
  end),
  ok.

%% Add the initial root user
%% email: root@getbeehive.com
%% password: 098f6bcd4621d373cade4e832627b4f6
add_root_user() ->
  create(new([
    {email, "root@getbeehive.com"},
    {password, "test"},
    {level, ?ADMIN_USER_LEVEL}
  ])).

from_proplists(Proplists) -> from_proplists(Proplists, #user{}).
from_proplists([], User)  -> User;
from_proplists([{email, V}|Rest], User) ->
  from_proplists(Rest, User#user{email = V});
from_proplists([{password, V}|Rest], User) ->
  from_proplists(Rest, User#user{password = V});
from_proplists([{key, V}|Rest], User) ->
  from_proplists(Rest, User#user{key = V});
from_proplists([{token, V}|Rest], User) ->
  from_proplists(Rest, User#user{token = V});
from_proplists([{level, V}|Rest], User) ->
  from_proplists(Rest, User#user{level = V});
from_proplists([_Other|Rest], User) ->
  from_proplists(Rest, User).

%% to_proplist(User) -> to_proplist(record_info(fields, user), User, []).
%% to_proplist([], _User, Acc) -> Acc;
%% to_proplist([_Other|Rest], User, Acc) -> to_proplist(Rest, User, Acc).

validate_user(User) when is_record(User, user) ->
  validate_user(record_info(fields, user), User).
validate_user([], User) ->  User;
%% Validate the name
validate_user([email|_Rest], #user{email = undefined} = _User) ->
  throw({error, {invalid_user, no_email_given}});
validate_user([email|Rest], #user{email = Email} = User) ->
  case validate_email(Email) of
    ok -> validate_user(Rest, User);
    Error -> throw({error, {invalid_user, invalid_email, Error}})
  end;

validate_user([password|Rest], #user{password = undefined} = User) ->
  validate_user(Rest, User#user{password = date_util:now_to_seconds()});
validate_user([key|Rest], User) -> validate_user(Rest, User);
validate_user([token|Rest], User) -> validate_user(Rest, User);
%% Validate level
validate_user([level|Rest], #user{level = undefined} = User) ->
  validate_user(Rest, User#user{level = ?REGULAR_USER_LEVEL});
validate_user([level|Rest], #user{level = Level} = User) when is_integer(Level) ->
  validate_user(Rest, User#user{level = Level});
validate_user([level|Rest], #user{level = Level} = User) when is_list(Level) ->
  validate_user(Rest, User#user{level = list_to_integer(Level)});
validate_user([_|Rest], User) -> validate_user(Rest, User).

%% Validate the email (STUB FOR NOW)
validate_email(_Email) -> ok.
