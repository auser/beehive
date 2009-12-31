%%%-------------------------------------------------------------------
%%% File    : home_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (home_controller).
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  {struct, [{"beehive", ?BINIFY(["apps", "nodes", "bees", "stats", "users"])}]}.

post(["_reload"], Data) ->
  auth_utils:run_if_admin(fun(_) ->
    misc_utils:reload_all()
  end, Data);
post(_Path, _Data) -> error("unhandled").
put(_Path, _Data) -> error("unhandled").
delete(_Path, _Data) -> error("unhandled").

error(Msg) ->
  {struct, [{error, misc_utils:to_bin(Msg)}]}.