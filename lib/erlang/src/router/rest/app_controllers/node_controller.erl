%%%-------------------------------------------------------------------
%%% File    : node_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (node_controller).
-include ("router.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  {struct, [
    {"routers", format_nodes(fun node_manager:get_routers/0)},
    {"nodes", format_nodes(fun node_manager:get_nodes/0)}
    ]
  }.
  
format_nodes(F) ->
  lists:append(
    lists:map(fun(#node{name = Name, host = Host} = _Node) ->
      {struct, ?BINIFY([
        {"name", Name},
        {"host", Host}
      ])}
    end, lists:map(fun(N) -> node_manager:dump(N) end, F())
  )).

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".