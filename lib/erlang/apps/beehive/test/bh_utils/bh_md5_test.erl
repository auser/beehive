%%%-------------------------------------------------------------------
%%% File    : md5_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec 17 13:12:13 PST 2009
%%%-------------------------------------------------------------------

-module (bh_md5_test).

-include_lib("eunit/include/eunit.hrl").

bh_md5_test_() ->
  {inparallel,
    [
      ?_assertEqual( bh_md5:hex("hello world"), "5eb63bbbe01eeed093cb22bb8f5acdc3"),
      ?_assertEqual(bh_md5:hex(["hello world"]), bh_md5:hex(["hello ", "world"]))
    ]
  }.
