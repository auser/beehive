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
-export ([get/2, post/2, put/2, delete/2]).

get(["mine"], _Data) ->  
  {struct, ?BINIFY([
      {"node", "mine"},
      {"cpu", lists:reverse(bh_node_stats_srv:node_dump(cpu, 10))},
      {"memory", bh_node_stats_srv:node_dump(mem, 10)}
    ]
  )};
get(["mine", RangeList], _Data) ->  
  Range = misc_utils:to_integer(RangeList),
  {struct, ?BINIFY([
      {"node", "mine"},
      {"cpu", bh_node_stats_srv:node_dump(cpu, Range)},
      {"memory", bh_node_stats_srv:node_dump(mem, Range)}
    ]
  )};
get(_, _Data) -> 
  {struct, [
    {"routers", format_nodes(fun node_manager:get_routers/0)},
    {"bees", format_nodes(fun node_manager:get_nodes/0)},
    {"storage", format_nodes(fun node_manager:get_storage/0)}
    ]
  }.
  
format_nodes(F) ->
  lists:map(fun(#node{name = Name, host = Host} = _Node) ->
      {struct, ?BINIFY([
        {"name", Name},
        {"host", Host}
      ])}
    end, lists:map(fun(N) -> node_manager:dump(N) end, F())
  ).

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".