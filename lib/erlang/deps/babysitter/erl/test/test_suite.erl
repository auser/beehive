-module (test_suite).

-include_lib("eunit/include/eunit.hrl").
 
all_test_() ->
  [
    {module, babysitter_test},
    {module, babysitter_parser_test},
    {module, babysitter_list_utils_test},
    {module, babysitter_config_test},
    {module, babysitter_load_test}
  ].
