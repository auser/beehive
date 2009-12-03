%%%-------------------------------------------------------------------
%%% File    : bh_receive_pack.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Based on Tom Preston-Werner's egitd (http://github.com/mojombo/egitd)
%%% Created :  Wed Dec  2 20:09:05 PST 2009
%%%-------------------------------------------------------------------

-module (bh_receive_pack).

-export([handle/3]).

handle(Sock, _Host, _Header) ->
  gen_tcp:send(Sock, "003d\n*********'\n\nYou can't push to this respository\n\n*********"),
  ok = gen_tcp:close(Sock).