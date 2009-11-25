%%%-------------------------------------------------------------------
%%% File    : bee_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:59:03 PST 2009
%%%-------------------------------------------------------------------

-module (bees_controller).
-include ("router.hrl").
-include ("http.hrl").

-export ([get/1, post/2, put/2, delete/2]).

get(["all"]) -> 
  All = bee:all(),
  {struct, [{
    "bees",
    lists:map(fun(B) ->
      {struct, ?BINIFY([
        {"app_name", B#bee.app_name},
        {"host", B#bee.host},
        {"port", B#bee.port},
        {"status", B#bee.status}]
      )}
    end, All)
  }]};
get(_Path) ->
  {struct, [{"path", <<"Backends">>}]}.

post(_Path, Data) ->
  case bee:create(Data) of
    {ok, Be} -> misc_utils:to_bin(io_lib:format("Added bee: ~p on ~p:~p", [Be#bee.app_name, Be#bee.host, Be#bee.port]));
    _ -> misc_utils:to_bin("There was an error adding bee\n")
  end.
  
% post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".

delete([Name], _Data) ->
  case bee:delete(Name) of
    ok -> misc_utils:to_bin("Deleted bee");
    _ -> misc_utils:to_bin("There was an error deleting bee\n")
  end;
delete([Name, Host, Port], _Data) ->
  case bee:delete(Name, Host, misc_utils:to_integer(Port)) of
    ok -> misc_utils:to_bin("Deleted bee");
    _ -> misc_utils:to_bin("There was an error deleting bee\n")
  end;
delete(_Path, _Data) -> "unhandled".