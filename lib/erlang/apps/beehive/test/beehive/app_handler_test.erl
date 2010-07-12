-module (app_handler_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

starting_test_() ->
  % These are longer tests
  {timeout, 900000,
    % {setup,
    %   fun setup/0,
    %   fun teardown/1,
      [
        fun start_new_instance_test/0,
        fun update_an_instance_test/0,
        fun teardown_an_instance_test/0
      ]
    % }
  }.

start_new_instance_test() ->  
  bh_test_util:setup(),
  App = dummy_app(),
  
  AppLauncher = self(),
  From = self(),
  app_handler:start_new_instance(App, "test", AppLauncher, From),
  receive
    {started_bee, Bee} ->
      timer:sleep(2000), % give them sometime to start up
      {ok, Code, Body} = bh_test_util:get_url([{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}]),
      ?assert(Code =:= 200),
      ?assert(Body =:= "you win"),
      kill_app_by_bee(App, Bee)
    after 5000 ->
      erlang:display({error, timeout})
  end,
  passed.

update_an_instance_test() ->
  passed.
  
teardown_an_instance_test() ->
  passed.


kill_app_by_bee(_App, #bee{pid = Pid} = _Bee) when Pid > 1 ->
  % spawn(fun() -> babysitter_integration:command(stop, App, Bee, []) end),
  Pid ! {stop};
kill_app_by_bee(_App, _Bee) -> ok.

dummy_app() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ReposDir = filename:join([Dir, "test", "fixtures", "dummy_srv"]),
  ReposUrl = lists:concat(["file://", ReposDir])
  
  {ok, App} = case apps:find_by_name("test_app") of
    not_found ->
      AppC = #app{name = "test_app", url = ReposUrl},
      apps:create(AppC);
    App1 ->
      {ok, App1}
  end,
  App.