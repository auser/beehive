-module (test_suite).

-include_lib("eunit/include/eunit.hrl").
 
all_test_() ->
  [
    {module, eunit_example_cluster_srv}
  ].
