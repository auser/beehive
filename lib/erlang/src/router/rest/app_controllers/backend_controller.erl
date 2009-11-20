%%%-------------------------------------------------------------------
%%% File    : backend_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:59:03 PST 2009
%%%-------------------------------------------------------------------

-module (backend_controller).
-include ("router.hrl").
-include ("http.hrl").

-export ([get/1, post/2, put/2, delete/2]).

get(["all"]) -> 
  All = backend:all(),
  {struct, [{
    "backends",
    lists:map(fun(B) ->
      {struct, ?BINIFY([
        {"app_name", B#backend.app_name},
        {"host", B#backend.host},
        {"port", B#backend.port},
        {"status", B#backend.status}]
      )}
    end, All)
  }]};
get(_Path) ->
  {struct, [{"path", <<"Backends">>}]}.

post(_Path, Data) ->
  case backend:create(Data) of
    {ok, Be} -> io_lib:format("Added backend: ~p on ~p:~p", [Be#backend.app_name, Be#backend.host, Be#backend.port]);
    _ -> "There was an error adding backend\n"
  end.
  
% post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".

delete([Name], _Data) ->
  case backend:delete(Name) of
    ok -> "Deleted backend";
    _ -> "There was an error deleting backend\n"
  end;
delete([Name, Host, Port], _Data) ->
  case backend:delete(Name, Host, misc_utils:to_integer(Port)) of
    ok -> "Deleted backend";
    _ -> "There was an error deleting backend\n"
  end;
delete(_Path, _Data) -> "unhandled".