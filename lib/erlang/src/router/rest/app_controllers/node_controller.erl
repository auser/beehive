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
  [
    "<h1>Nodes</h1>",
    "<table>\n",
    "<tr><th>Router Nodes</th></tr>",
    format_nodes(fun node_manager:get_routers/0),
    "<tr><th>Nodes</th></tr>",
    format_nodes(fun node_manager:get_nodes/0),
    "</table>\n"
  ].
  
format_nodes(F) ->
  lists:append(
    [["<td><b>", X, "</b></td>"] || X <- ["Name", "Host"]],
    lists:map(fun(#node{name = Name, host = Host} = _Node) ->
      [
        "<tr>",
        io_lib:format("<td> ~s </td>", [Name]),
        io_lib:format("<td> ~s </td>", [Host]),
        "</tr>"
      ]
    end, lists:map(fun(N) -> node_manager:dump(N) end, F())
  )).

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".