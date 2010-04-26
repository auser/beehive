-module (app_handler_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  event_manager:start_link(),
  app_handler:start_link(),
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  CDir = filename:join([Dir, "fixtures", "configs"]),
  babysitter:start_link([{config_dir, CDir}]),
  ok.
  
teardown(_X) ->
  event_manager:stop(),
  app_handler:stop(),
  babysitter:stop(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun initialize_application_test/0,
        fun can_deploy_new_app_test/0
      ]
    }
  }.

can_deploy_new_app_test() ->
  ?assert(app_handler:can_deploy_new_app()),
  ok.

initialize_application_test() ->
  Self = self(),
  app_handler:start_new_instance(dummy_app(), "shashasha", Self, Self),
  ?assert(false),
  ok.

dummy_app() ->
  #app{
    name="Dummy app"
  }.