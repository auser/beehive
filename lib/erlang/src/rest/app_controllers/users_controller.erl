%%%-------------------------------------------------------------------
%%% File    : users_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sat Nov 28 23:03:51 PST 2009
%%%-------------------------------------------------------------------

-module (users_controller).

-include ("beehive.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  All = users:all(),
  {struct, [{
    "users",
    lists:map(fun(A) ->
      {struct, ?BINIFY([
        {"email", A#user.email}
      ])}
    end, All)
  }]}.

post(["new"], Data) ->
  case users:create(Data) of
    User when is_record(User, user) -> 
      {struct, ?BINIFY([{"user", misc_utils:to_bin(User#user.email)}])};
    _ -> "There was an error adding bee\n"
  end;
    
post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".