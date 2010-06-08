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
  [
    {"node", "mine"},
    {"cpu", lists:reverse(bh_node_stats_srv:node_dump(cpu, 10))},
    {"memory", bh_node_stats_srv:node_dump(mem, 10)}
  ];
get(["mine", RangeList], _Data) ->  
  Range = misc_utils:to_integer(RangeList),
  [
    {"node", "mine"},
    {"cpu", bh_node_stats_srv:node_dump(cpu, Range)},
    {"memory", bh_node_stats_srv:node_dump(mem, Range)}
  ];
get(_, _Data) -> 
  [
    {"routers", format_nodes(node_manager:get_servers(router))},
    {"nodes", format_nodes(node_manager:get_servers(node))},
    {"storage", format_nodes(node_manager:get_servers(storage))}
  ].
  
format_nodes(List) -> format_nodes(List, []).
format_nodes([], Acc) -> Acc;
format_nodes([H|Rest], Acc) -> format_nodes(Rest, [format_node(H)|Acc]).

format_node(Pid) when is_pid(Pid) ->
  #node{name = Name, host = Host} = node_manager:dump(Pid),
  [
    {"name", Name},
    {"host", Host}
  ].

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".