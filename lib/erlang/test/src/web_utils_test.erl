%%%-------------------------------------------------------------------
%%% File    : web_utils_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Jan  4 19:03:02 PST 2010
%%%-------------------------------------------------------------------

-module (web_utils_test).

-include_lib("eunit/include/eunit.hrl").

query_to_proplist_test_() ->
  [
    ?_assertEqual(web_utils:query_params_to_proplist(""), []),
    ?_assertEqual(web_utils:query_params_to_proplist("token=hi"), [{token, "hi"}]),
    ?_assertEqual(web_utils:query_params_to_proplist("token"), []),
    ?_assertEqual(web_utils:query_params_to_proplist("token="), [{token, ""}]),
    ?_assertEqual(web_utils:query_params_to_proplist("token=hi&bears=boxing"), [{token, "hi"}, {bears, "boxing"}])
  ].
