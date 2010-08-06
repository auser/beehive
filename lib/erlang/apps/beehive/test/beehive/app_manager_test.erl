-module (app_manager_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  erlang:display({setup, app_manager_test}),
  bh_test_util:setup(),
  erlang:display({after_setup}),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun instance/0,
        fun add_application/0,
        fun spawn_update_bee_status/0,
        fun start_new_instance/0,
        fun teardown_an_instance/0
      ]
    }
  }.

instance()->
  ?assert(undefined =/= app_manager:instance()),
  passed.

add_application() ->
  bh_test_util:delete_all(app),
  User = bh_test_util:dummy_user(),
  O = app_manager:add_application([{name, "bobby-bobbie-o"}], User),
  ?assert(element(1, O) =:= ok),
  passed.

spawn_update_bee_status() ->
  passed.

% Starting and stopping
start_new_instance() ->  
  {ok, App, Bee} = start_dummy_app(self()),
  timer:sleep(1000),
  case try_to_fetch_url_or_retry(get, [{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}], 20) of
    {ok, _Headers, Body} ->
      ?assertEqual("you win", hd(lists:reverse(Body))),
      passed;
    _ -> 
      ?assertEqual(failed, connect)
  end,
  kill_app_by_bee(App, Bee),
  passed.
  
teardown_an_instance() ->
  {ok, _App, Bee} = start_dummy_app(self()),
  timer:sleep(1000),
  app_manager:stop_instance(Bee, self()),
  receive
    {bee_terminated, _Bee} ->
      timer:sleep(1000),
      case catch gen_tcp:connect(Bee#bee.host, Bee#bee.port, [binary]) of
        {ok, _Sock} -> ?assert(false);
        {error,econnrefused} -> ?assert(true)
      end;
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
  app_manager:request_to_start_new_bee_by_app(App),
  receive
    {started_bee, Bee} ->
      bees:save(Bee),
      {ok, App, Bee}
    after 5000 ->
      {error, timeout}
  end.

try_to_fetch_url_or_retry(_Method, _Args, 0) -> failed;
try_to_fetch_url_or_retry(Method, Args, Times) ->
  case bh_test_util:fetch_url(Method, Args) of
    {ok, _Headers, _Body} = T -> T;
    _E -> try_to_fetch_url_or_retry(Method, Args, Times - 1)
  end.