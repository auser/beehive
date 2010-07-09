-module (app_handler_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  test_util:setup_test(),
  ok.
  
teardown(_X) ->
  test_util:teardown_test(),
  ok.


starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun start_new_instance_test/0
      ]
    }
  }.

start_new_instance_test() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ReposDir = filename:join([Dir, "test", "fixtures", "dummy_srv"]),
  ReposUrl = lists:concat(["file://", ReposDir]),
  AppC = #app{name = "test_app", url = ReposUrl},
  {ok, App} = apps:create(AppC),
  % erlang:display({repos, ReposUrl}),
  AppLauncher = self(),
  From = self(),
  app_handler:start_new_instance(App, "test", AppLauncher, From),
  % start_new_instance(App, Sha, AppLauncher, From) ->
  passed.