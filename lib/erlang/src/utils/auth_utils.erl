%%%-------------------------------------------------------------------
%%% File    : auth_utils.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:15:35 PST 2009
%%%-------------------------------------------------------------------

-module (auth_utils).
-include ("beehive.hrl").
-compile(export_all).

% Only run if there is a user associated with given token
get_authorized_user(Data) ->
  case proplists:is_defined(token, Data) of
    false -> false;
    true ->
      Token = proplists:get_value(token, Data),
      case users:find_by_token(Token) of
        [] -> false;
        User -> User
      end
  end.

run_if_admin(F, Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> misc_utils:to_bin("Unauthorized");
    ReqUser ->
      case auth_utils:is_admin_user(ReqUser) of
        false -> misc_utils:to_bin("Unauthorized");
        true -> F()
      end
  end.

is_admin_user(User) ->
  User#user.level < ?REGULAR_USER_LEVEL.