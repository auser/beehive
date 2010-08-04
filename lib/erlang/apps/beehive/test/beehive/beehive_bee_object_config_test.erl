-module (beehive_bee_object_config_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "fixtures", "beehive.cfg"]),
  
  application:set_env(beehive, config_file, ConfigFile),
  beehive_bee_object:init(),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun reading/0,
        fun error/0,
        fun get_or_default_t/0
      ]
    }
  }.

reading() ->
  ?assert(lists:member(bundle, beehive_bee_object_config:list_configs())),
  ?assert(lists:member(default, beehive_bee_object_config:list_actions(bundle))),
  Str = beehive_bee_object_config:get(bundle, default),
  ?assertEqual(string:str(Str, "#!/bin"), 1),
  passed.

error() ->
  ?assertEqual({error, not_found}, beehive_bee_object_config:get(bundle, java)),
  passed.

get_or_default_t() ->
  Str = beehive_bee_object_config:get_or_default(bundle, java),
  ?assertEqual(string:str(Str, "#!/bin"), 1),
  passed.