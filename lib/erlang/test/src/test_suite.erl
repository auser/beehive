-module (test_suite).

-include_lib("eunit/include/eunit.hrl").
 
all_test_() ->
  [
    {module, bh_md5_test},
    {module, string_test},
    {module, bh_host_test},
    {module, web_utils_test},
    {module, misc_utils_test}
  ].
