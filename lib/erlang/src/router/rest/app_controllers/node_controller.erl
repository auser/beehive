%%%-------------------------------------------------------------------
%%% File    : node_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (node_controller).
-include ("router.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  Nodes = node_manager:list_nodes(),
  [
    "<h1>Nodes</h1>",
    "<table>\n",
    "<tr> ",
    [["<td><b>", X, "</b></td>"] || X <- ["Name", "Ping latency", "Host", "Type"]],
    lists:map(fun([{Name, Node}]) ->
      [
        "<tr>",
        io_lib:format("<td> ~s </td>", [Name]),
        io_lib:format("<td> ~w </td>", [Node#node.ping_distance]),
        io_lib:format("<td> ~s </td>", [Node#node.host]),
        io_lib:format("<td> ~s </td>", [Node#node.type]),
        "</tr>"
      ]
    end, Nodes),
    "</table>\n"
  ].

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".