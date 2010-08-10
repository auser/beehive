-module (app_manager_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  bh_test_util:setup(),
  ok.
  
teardown(_X) ->
  ok.

all_test_() ->
  Tests = {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun instance/0,
        fun add_application/0,
        fun spawn_update_bee_status/0,
        fun start_new_instance_t/0,
        fun teardown_an_instance_t/0
      ]
    }
  },
  {timeout, 60, Tests}.

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
start_new_instance_t() ->  
  {ok, App, Bee} = start_dummy_app(self()),
  timer:sleep(500),
  erlang:display({start_new_instance, Bee#bee.host, Bee#bee.port, Bee#bee.os_pid}),
  case try_to_fetch_url_or_retry(get, [{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}], 20) of
    {ok, _Headers, Body} ->
      ?assertEqual("Hello World", hd(lists:reverse(Body))),
      % os:cmd(lists:flatten(["kill ", integer_to_list(Bee#bee.os_pid)])),
      passed;
    _ -> 
      ?assertEqual(failed, connect)
  end.
  
teardown_an_instance_t() ->
  erlang:display({teardown_an_instance_t}),
  % {ok, _App, Bee} = start_dummy_app(self()),
  App = dummy_app(),
  Bee = bees:find_by_name(App#app.name),
  app_manager:request_to_terminate_bee(Bee, self()),
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
    after 5000 ->
      erlang:display({error, timeout}),
      ?assert(something_went_wrong =:= true)
  end,
  passed.

dummy_app() -> bh_test_util:dummy_app().
  
start_dummy_app(From) ->
  App = dummy_app(),
  app_manager:request_to_start_new_bee_by_app(App, self()),
  receive
    {bee_started_normally, Bee} ->
      bees:save(Bee),
      {ok, App, Bee};
    X ->
      erlang:display({start_dummy_app, X}),
      ok
    after 5000 ->
      erlang:display({timeout}),
      throw({start_dummy_app, timeout})
  end.

try_to_fetch_url_or_retry(_Method, _Args, 0) -> failed;
try_to_fetch_url_or_retry(Method, Args, Times) ->
  case bh_test_util:fetch_url(Method, Args) of
    {ok, _Headers, _Body} = T -> T;
    _E -> try_to_fetch_url_or_retry(Method, Args, Times - 1)
  end.