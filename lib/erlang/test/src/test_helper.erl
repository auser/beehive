-module (test_helper).

-compile(export_all).

setup() ->
  node_manager:start_link(),
  event_manager:start_link(),
  app_handler:start_link(),
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  CDir = filename:join([Dir, "fixtures", "configs"]),
  babysitter:start_link([{config_dir, CDir}]),
  bh_storage_srv:start_link(),
  application:start(beehive),
  ok.

teardown() ->
  event_manager:stop(),
  app_handler:stop(),
  babysitter:stop(),
  bh_storage_srv:stop(),
  application:stop(beehive),
  ok.