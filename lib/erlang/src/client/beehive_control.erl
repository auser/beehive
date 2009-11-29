%%%-------------------------------------------------------------------
%%% File    : beehive_control.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Eventually, this will allow command-line access to the beehive
%%%   commands
%%% 
%%% Created :  Sun Nov 29 00:13:09 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_control).

-export ([start/0]).

start() ->
  RouterNode = misc_utils:localnode(router),
  
  io:format("RouterNode: ~p~n", [RouterNode]).