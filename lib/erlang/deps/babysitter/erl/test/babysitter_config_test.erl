-module (babysitter_config_test).
-include ("babysitter.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun read_test/0,
        fun parsing_non_existent_test/0,
        fun parsing_an_action_from_nonexistent_test/0,
        fun parsing_an_action_that_is_undefined/0,
        fun parsing_dir_test/0
      ]
    }
  }.

read_test() ->
  {ok, Files} = config_read(),
  ?assertEqual([default, java, rack], Files).

parsing_non_existent_test() ->
  config_read(),
  Lookup = ets:lookup(?BABYSITTER_CONFIG_DB, a_totally_unreal_application_configuration),
  ?assert(Lookup == []),
  ?assertEqual(babysitter_config:get_raw_config(a_totally_unreal_application_configuration), {error, not_found}).

parsing_an_action_from_nonexistent_test() ->
  config_read(),
  ?assertEqual({error, not_found}, babysitter_config:get(a_totally_unreal_application_configuration, bundle)).

parsing_an_action_that_is_undefined() ->
  config_read(),
  ?assertEqual({error, no_action}, babysitter_config:get(rack, a_totally_unrealistic_and_not_recognized_action_state)).

parsing_dir_test() ->
  config_read(),
  {ok, BCLookup} = babysitter_config:get(rack, bundle),
  [{rack, EtsLookup}] = ets:lookup(?BABYSITTER_CONFIG_DB, rack),
  BundleLookup = proplists:get_value(bundle, EtsLookup),
  ?assertEqual(BundleLookup, BCLookup).

% Internal functions
config_read() ->
  Dir = filename:dirname(code:which(?MODULE)),
  ConfigDir = filename:join([Dir, "..", "..", "config", "apps"]),
  babysitter_config:read(ConfigDir).