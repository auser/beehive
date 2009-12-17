-module (test_suite).

-include_lib("eunit/include/eunit.hrl").
 
all_test_() ->
  [
    {module, md5_test},
    {module, string_test}
  ].
