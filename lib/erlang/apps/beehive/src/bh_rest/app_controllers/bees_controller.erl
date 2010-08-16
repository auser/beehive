%%%-------------------------------------------------------------------
%%% File    : bee_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:59:03 PST 2009
%%%-------------------------------------------------------------------

-module (bees_controller).
-include ("beehive.hrl").
-include ("http.hrl").

-export ([get/2, post/2, put/2, delete/2]).

get(_, _Data) -> 
  All = bees:all(),
  O = {
    "bees",
    lists:map(fun(B) ->
      [
        {"app_name", B#bee.app_name},
        {"host", B#bee.host},
        {"port", B#bee.port},
        {"commit", B#bee.revision},
        {"status", B#bee.status}
      ]
    end, All)
  },
  O.

post(_Path, Data) ->
  case bees:create(Data) of
    {ok, Be} -> 
      Message = misc_utils:to_bin(lists:append(["Added bee ", Be#bee.app_name])),
      {"message", Message};
    {error, M} -> 
      {"error", misc_utils:to_bin(misc_utils:to_list(M))}
  end.
  
% post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".

delete([Name], _Data) ->
  case bees:delete(Name) of
    ok -> misc_utils:to_bin("Deleted bee");
    _ -> misc_utils:to_bin("There was an error deleting bee\n")
  end;
delete([Name, Host, Port], _Data) ->
  case bees:delete(Name, Host, misc_utils:to_integer(Port)) of
    ok -> misc_utils:to_bin("Deleted bee");
    _ -> misc_utils:to_bin("There was an error deleting bee\n")
  end;
delete(_Path, _Data) -> "unhandled".
