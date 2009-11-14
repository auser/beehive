%%%-------------------------------------------------------------------
%%% File    : node_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (node_controller).

-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  Nodes = node_manager:list_nodes(),
  [
    "<h1>Nodes</h1>",
    "<table>\n",
    "<tr> ",
    [["<td><b>", X, "</b></td>"] || X <- ["Name"]],
    lists:map(fun([{Name, _Node}]) ->
      [
        "<tr>",
        io_lib:format("<td> ~s </td>", [Name]),
        "</tr>"
      ]
    end, Nodes),
    "</table>\n"
  ].

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".