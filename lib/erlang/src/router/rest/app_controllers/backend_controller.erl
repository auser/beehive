%%%-------------------------------------------------------------------
%%% File    : backend_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:59:03 PST 2009
%%%-------------------------------------------------------------------

-module (backend_controller).
-include ("router.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(["all"]) -> 
  All = backend:all(),
  [
    "<h2>All Backends</h2>",
    "<table>\n",
    "<tr> ",
    [["<td><b>", X, "</b></td>"] || X <- [
      "Name", "Host", "Port", "Status"
    ]],
    "\n",
    lists:map(fun(Backend) ->
      [
        "<tr>",
        io_lib:format("<td> ~s </td>", [Backend#backend.app_name]),
        io_lib:format("<td> ~s </td>", [Backend#backend.host]),
        io_lib:format("<td> ~w </td>", [Backend#backend.port]),
        io_lib:format("<td> ~s </td>", [Backend#backend.status]),
        "</tr>"
      ]
    end, All),
    "</table>\n"
  ];
get(_Path) ->
  "Backends".

post(_Path, Data) ->
  case backend:create(Data) of
    ok -> "Added backend";
    _ -> "There was an error adding backend\n"
  end.
  
% post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".