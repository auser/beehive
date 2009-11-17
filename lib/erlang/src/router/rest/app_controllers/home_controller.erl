%%%-------------------------------------------------------------------
%%% File    : home_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (home_controller).

-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  [
    "<h3>Beehive</h3>",
    "<table id='table'>",
    "<tr>",
    "<td><a href='/node'>Nodes</a></td>",
    "<td><a href='/app'>Applications</a></td>",
    "<td><a href='/stats'>Stats</a></td>",
    "<td><a href='/backend'>Backend</a></td>",
    "</tr>",
    "</table>"
  ].

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".