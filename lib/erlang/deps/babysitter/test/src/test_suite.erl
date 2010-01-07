-module (test_suite).

-include_lib("eunit/include/eunit.hrl").
 
all_test_() ->
  [
    {module, babysitter_test}
  ].
