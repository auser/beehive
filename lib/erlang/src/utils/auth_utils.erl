%%%-------------------------------------------------------------------
%%% File    : auth_utils.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:15:35 PST 2009
%%%-------------------------------------------------------------------

-module (auth_utils).

-compile(export_all).

% Only run if there is a user associated with given token
is_authorized_request(Data) ->
  case proplists:is_defined(token, Data) of
    false -> false;
    true ->
      Token = proplists:get_value(token, Data),
      case users:find_by_token(Token) of
        [] -> false;
        _ -> true
      end
  end.
