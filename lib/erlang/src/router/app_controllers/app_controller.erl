%%%-------------------------------------------------------------------
%%% File    : app_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:43 PST 2009
%%%-------------------------------------------------------------------

-module (app_controller).
-include ("router.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(["all"]) -> 
  All = app:all(),
  [
    "<h2>All Apps</h2>",
    "<table>\n",
    "<tr> ",
    [["<td><b>", X, "</b></td>"] || X <- [
      "Name", "Url", "LastUpdated"
    ]],
    "\n",
    lists:map(fun(App) ->
      [
        "<tr>",
        io_lib:format("<td> ~s </td>", [App#app.name]),
        io_lib:format("<td> ~s </td>", [App#app.url]),
        io_lib:format("<td> ~s </td>", [date_util:fmt_date(App#app.updated_at)]),
        "</tr>"
      ]
    end, All),
    "</table>\n"
  ];

get(_) -> "hello world".

post(["new"], Data) ->
  case app:create(Data) of
    ok -> "Added new app";
    _ -> "There was an error adding backend\n"
  end;
  
post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".