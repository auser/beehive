%%%-------------------------------------------------------------------
%%% File    : node_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (nodes_controller).
-include ("beehive.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get([Name]) ->  
  {struct, ?BINIFY([
      {"node", Name},
      {"cpu", stats_srv:node_dump(cpu, 10)},
      {"memory", stats_srv:node_dump(mem, 10)}
    ]
  )};
get([Name, RangeList]) ->  
  Range = misc_utils:to_integer(RangeList),
  {struct, ?BINIFY([
      {"node", Name},
      {"cpu", stats_srv:node_dump(cpu, Range)},
      {"memory", stats_srv:node_dump(mem, Range)}
    ]
  )};
get(_) -> 
  {struct, [
    {"routers", [format_nodes(fun node_manager:get_routers/0)]},
    {"nodes", format_nodes(fun node_manager:get_nodes/0)},
    {"storage", format_nodes(fun node_manager:get_storage/0)}
    ]
  }.
  
format_nodes(F) ->
  Nodes = lists:append(
    lists:map(fun(#node{name = Name, host = Host} = _Node) ->
      ?BINIFY([
        {"name", Name},
        {"host", Host}
      ])
    end, lists:map(fun(N) -> node_manager:dump(N) end, F())
  )),
  {struct, Nodes}.

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".