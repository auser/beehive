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
  add_root_user/0,
  find_by_email/1,
  find_all_by_email/1,
  all/0,
  exist/1,
  create/1,
  update/1,
  delete/1, 
  create_new_token_for/1, create_new_token_for/2,
  find_by_token/1,
  is_user_token/2,
  new/1
]).

find_by_email(Hostemail) ->
  case find_all_by_email(Hostemail) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_email(Name) -> 
  db:read({user, Name}).

find_by_token(Token) ->
  case db:match(#user{token = Token, _='_'}) of
    [U|_] -> U;
    _ -> []
  end.

% Does this user exist?
exist(Name) ->
  case find_by_email(Name) of
    [] -> false;
    _ -> true
  end.

% Insert a new user
create(User) when is_record(User, user) ->
  case db:write(User) of
    {'EXIT', {aborted, {no_exists, user}}} -> 
      ?NOTIFY({db, database_not_initialized, bee}),
      {error, database_not_initialized};
    {'EXIT', _} -> unknown_error;
    ok -> 
      case exist(User#user.email) of
        true -> ?NOTIFY({user, updated, User});
        false -> ?NOTIFY({user, created, User})
      end,
      User
  end;
  
create(NewProps) ->
  create(new(NewProps)).

update(NewProps) ->
  create(new(NewProps)).

delete(User) when is_record(User, user) -> db:delete_object(User);
delete(Name) ->
  db:delete_object(#user{email=Name, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(user) ])).

create_new_token_for(User) when is_record(User, user) ->
  NewToken = bh_md5:hex(lists:flatten([
    User#user.email,
    misc_utils:to_list(date_util:now_to_seconds())
  ])),
  create(User#user{token = NewToken}).

is_user_token(Email, Token) ->
  case find_by_email(Email) of
    [] -> false;
    User ->
      User#user.token =:= Token
  end.
  

% Create a new token for the user
create_new_token_for(Email, Password) ->
  case find_by_email(Email) of
    [] -> error;
    User ->
      case User#user.password =:= bh_md5:hex(Password) of
        false -> error;
        true ->
          create_new_token_for(User)
      end
  end.
  

% Add the initial root user
% email: root@getbeehive.com
% password: 098f6bcd4621d373cade4e832627b4f6
add_root_user() ->
  create(new([
    {email, "root@getbeehive.com"},
    {password, "test"},
    {level, ?ADMIN_USER_LEVEL}
  ])).

new(NewProps) ->
  PropList = ?rec_info(user, #user{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  FilteredProplist = validate_user_proplists(FilteredProplist2),
  list_to_tuple([user|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, user)]]).

validate_user_proplists(PropList) ->
  lists:map(fun({Key, Val}) ->
    case Key of
      updated_at -> {Key, date_util:now_to_seconds()};
      password -> 
        case Val of
          undefined -> bh_md5:hex("test");
          _ -> {Key, bh_md5:hex(Val)}
        end;
      token -> {Key, none};
      level -> 
        Lvl = case Val of
          undefined -> ?REGULAR_USER_LEVEL;
          T -> misc_utils:to_integer(T)
        end,
        {Key, Lvl};
      _ -> {Key, Val}
    end
  end, PropList).
