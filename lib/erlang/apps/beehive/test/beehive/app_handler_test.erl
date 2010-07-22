-module (app_handler_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  bh_test_util:setup(),
  ok.
  
teardown(_X) ->
  ok.
  
starting_test_() ->
  % These are longer tests
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun start_new_instance_test/0,
        fun teardown_an_instance_test/0
      ]
    }
  }.

start_new_instance_test() ->  
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
  
teardown_an_instance_test() ->
  {ok, _App, Bee} = start_dummy_app(self()),
  timer:sleep(1000),
  app_handler:stop_instance(Bee, self()),
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
  app_handler:start_new_instance(App, "test", From, From),
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