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

% Call the function if the user (defined by the token) is
% found and their email is in the list of given emails
% otherwise chuck out as unauthorized
run_if_authorized(F, Emails, Data) ->
  case get_authorized_user(Data) of
    false -> misc_utils:to_bin("No user defined or invalid token");
    ReqUser ->
      case lists:member(ReqUser#user.email, Emails) of
        false -> misc_utils:to_bin("Unauthorized user");
        true -> F(ReqUser)
      end
  end.

% If the user is defined by the token and the user is an admin, then
% call the function With the authorized user record
run_if_admin(F, Data) ->
  case are_there_users() of
    false ->
      F([]);
    true ->
      case get_authorized_user(Data) of
        false -> misc_utils:to_bin("Unauthorized");
        ReqUser ->
          case is_admin_user(ReqUser) of
            false -> misc_utils:to_bin("Unauthorized");
            true -> F(ReqUser)
          end
      end
  end.

% Check to make sure there are actually users in the system. If there aren't, 
% any commands will do
are_there_users() ->
  case length(users:all()) of
    0 -> false;
    _ -> true
  end.


is_admin_user(User) ->
  User#user.level < ?REGULAR_USER_LEVEL.