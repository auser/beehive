-module (app_handler_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(),
  ok.
  
teardown(_X) ->
  bh_test_util:teardown(),
  ok.
  
starting_test_() ->
  % These are longer tests
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun start_new_instance_test/0,
        fun update_an_instance_test/0,
        fun teardown_an_instance_test/0
      ]
    }
  }.

start_new_instance_test() ->  
  {ok, App, Bee} = start_dummy_app(self()),
  timer:sleep(1000),
  case bh_test_util:get_url([{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}]) of
    {ok, Code, Body} ->
      ?assert(Code =:= 200),
      ?assert(Body =:= "you win"),
      kill_app_by_bee(App, Bee),
      passed;
    E ->
      erlang:display({error, start_new_instance_test, E}),
      failed
  end.

update_an_instance_test() ->
  passed.
  
teardown_an_instance_test() ->
  {ok, _App, Bee} = start_dummy_app(self()),
  timer:sleep(1000),
  app_handler:stop_instance(Bee, self()),
  receive
    {bee_terminated, _Bee} ->
      timer:sleep(1000),
      C = bh_test_util:get_url([{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}]),
      ?assert(element(1, C) =:= error);
    T ->
      erlang:display({else, T}),
      ?assert(something_went_wrong =:= true)
  end,
  passed.


kill_app_by_bee(_App, #bee{pid = Pid} = _Bee) when Pid > 1 ->
  % spawn(fun() -> babysitter_integration:command(stop, App, Bee, []) end),
  Pid ! {stop};
kill_app_by_bee(_App, _Bee) -> ok.

dummy_app() -> bh_test_util:dummy_app().
  
start_dummy_app(From) ->
  App = dummy_app(),
  app_handler:start_new_instance(App, "test", From, From),
  receive
    {started_bee, Bee} ->
      bees:save(Bee),
      {ok, App, Bee}
    after 5000 ->
      {error, timeout}
  end.