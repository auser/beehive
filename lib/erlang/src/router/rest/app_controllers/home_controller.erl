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
  {struct, [{"beehive", <<"app, node, backend, stats">>}]}.
post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".