%%%-------------------------------------------------------------------
%%% File    : auth_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 20:40:59 PST 2009
%%%-------------------------------------------------------------------

-module (auth_controller).
-include ("http.hrl").
-include ("beehive.hrl").

-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  {struct, [{"beehive", <<"app, node, bees, stats">>}]}.

post([], Data) ->
  io:format("Data: ~p~n", [Data]),
  case proplists:is_defined(email, Data) of
    false -> error("No email in auth request");
    true ->
      case proplists:is_defined(password, Data) of
        false -> error("No password in auth request");
        true ->
          Email = proplists:get_value(email, Data),
          Pass = proplists:get_value(password, Data),
          case users:create_new_token_for(Email, Pass) of
            User when is_record(User, user) ->
              {struct, ?BINIFY([{user, Email}, {token, User#user.token}])};
            _Else ->
              error("There was a problem authenticating")
          end
      end
  end;
  
post(_Path, _Data) -> "unhandled".

put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".

error(Msg) ->
  {struct, [{error, misc_utils:to_bin(Msg)}]}.