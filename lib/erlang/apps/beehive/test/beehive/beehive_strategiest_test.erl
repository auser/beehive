-module (beehive_strategiest_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun random/0,
        fun least_loaded/0
      ]
    }
  }.

random() ->  
  List = [1,2,3,4,5,b,c,d,a],
  Out = hd(bee_strategies:random(List)),
  ?assert(lists:member(Out, List)),
  passed.
  
least_loaded() ->
  List = [
    #bee{id={"chacha", {127,0,0,1}, 9001}},
    #bee{id={"meringue", {127,0,0,1}, 9002}},
    #bee{id={"keylime", {127,0,0,1}, 9003}}
  ],
  Out = hd(bee_strategies:least_loaded(List)),
  ?assert(lists:member(Out, List)),
  passed.
