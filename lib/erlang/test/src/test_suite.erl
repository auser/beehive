-module (test_suite).

-include_lib("eunit/include/eunit.hrl").
 
all_test_() ->
  [
    {module, bh_md5_test},
    {module, string_test}
  ].
