-module (app_handler_tests).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

starting_test_() ->
  % These are longer tests
  {timeout, 90000,
    % {setup,
    %   fun setup/0,
    %   fun teardown/1,
      [
        fun start_new_instance_test/0
      ]
    % }
  }.

start_new_instance_test() ->
  erlang:display({start_new_instance_test}),
  test_util:setup_test(),
  
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ReposDir = filename:join([Dir, "test", "fixtures", "dummy_srv"]),
  ReposUrl = lists:concat(["file://", ReposDir]),
  
  {ok, App} = case apps:find_by_name("test_app") of
    not_found ->
      AppC = #app{name = "test_app", url = ReposUrl},
      apps:create(AppC);
    App1 ->
      {ok, App1}
  end,
  % erlang:display({repos, ReposUrl}),
  AppLauncher = self(),
  From = self(),
  app_handler:start_new_instance(App, "test", AppLauncher, From),
  % start_new_instance(App, Sha, AppLauncher, From) ->
  % test_util:teardown_test(),
    
  passed.