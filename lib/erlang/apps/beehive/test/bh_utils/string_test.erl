%%%-------------------------------------------------------------------
%%% File    : string_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Dec 17 13:14:26 PST 2009
%%%-------------------------------------------------------------------

-module (string_test).

-include_lib("eunit/include/eunit.hrl").

subs_test_() ->
  [
    ?_assertEqual("Apples", string_utils:sub("apples", "a", "A")),
    ?_assertEqual("applEs", string_utils:sub("apples", "e", "E")),
    ?_assertEqual("apples", string_utils:sub("apples", "ducks", "fawns")),
    ?_assertEqual("apples", string_utils:sub("apples", "lest", "test")),
    ?_assertEqual("apeles", string_utils:sub("apples", "app", "ape"))
  ].

templated_string_test_() ->
  [
    ?_assertEqual("red apples", string_utils:template_command_string("[[ADJECTIVE]] apples",  [{"[[ADJECTIVE]]", "red"}])),
    ?_assertEqual("blue grapes", string_utils:template_command_string("[[ADJECTIVE]] [[NOUN]]",  [{"[[ADJECTIVE]]", "blue"}, {"[[NOUN]]", "grapes"}])),
    ?_assertEqual("[[ADJECTIVE]] apples", string_utils:template_command_string("[[ADJECTIVE]] apples",  [{"[[NOUN]]", "pear"}])),
    ?_assertEqual("[[ADJECTIVE]] apples", string_utils:template_command_string("[[ADJECTIVE]] apples",  [{"[[NOUN]]", "pear"}, {pickles, "pecans"}]))
  ].