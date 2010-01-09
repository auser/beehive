%%%-------------------------------------------------------------------
%%% File    : app_utils_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jan  8 15:58:14 PST 2010
%%%-------------------------------------------------------------------

-module (app_utils_test).

-include_lib("eunit/include/eunit.hrl").

template_parsing_test_() ->
  [
    ?_assertEqual([{a, "is_for_apples"}, {b, "is_for_bears"}], app_utils:template_proplists(
      [{a, "[[A]]"}, {b, "[[B]]"}],
      [{"[[A]]", "is_for_apples"}, {"[[B]]", "is_for_bears"}],
      []
    ))
  ].
