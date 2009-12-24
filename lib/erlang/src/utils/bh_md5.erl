%%%-------------------------------------------------------------------
%%% File    : md5.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Create an MD5 string from a string
%%% Created :  Sat Nov 28 22:57:01 PST 2009
%%%-------------------------------------------------------------------

-module (bh_md5).

-export([hex/1]).

hex(S) ->
  case io_lib:char_list(S) of
    false -> lists:append(S);
    true ->
      <<M:128>> = erlang:md5(S), 
      lists:flatten(io_lib:format("~32.16.0b", [M]))
  end.