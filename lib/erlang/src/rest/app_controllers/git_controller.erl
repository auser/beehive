%%%-------------------------------------------------------------------
%%% File    : git_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:18:32 PST 2009
%%%-------------------------------------------------------------------

-module (git_controller).

-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  {struct, [{"beehive", <<"app, node, bees, stats">>}]}.

post(["post-receive"], Data) ->
  io:format("Github post commit hook fired: ~p~n", [Data]),
  misc_utils:to_bin("Success!");
  
post(_Path, _Data) -> "unhandled".

put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".