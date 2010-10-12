-module (app_manager_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

all_test_app_manager_test_() ->
  bh_test_util:setup(),
  {timeout, 6000,
    [
        fun instance/0,
        fun add_application/0,
        fun spawn_update_bee_status/0,
        fun start_new_instance_t/0,
        % fun start_new_instance_t_failing_app/0,
        fun teardown_an_instance_t/0,
        fun cleanup/0
      ]
    }.

cleanup() ->
  lists:map(fun(Dir) ->
    bh_file_utils:rm_rf(filename:join([related_dir(), Dir]))
  end, ["run", "squashed"]),
  ok.

instance()->
  ?assert(undefined =/= app_manager:instance()),
  passed.

add_application() ->
  bh_test_util:delete_all(app),
  User = bh_test_util:dummy_user(),
  O = app_manager:add_application([{name, "bobby-bobbie-o"},
                                  {repo_url, bh_test_util:dummy_git_repos_url()}],
                                   User),
  ?assert(element(1, O) =:= ok),
  passed.

spawn_update_bee_status() ->
  passed.

% Starting and stopping
start_new_instance_t() ->
  {ok, App, Bee} = start_dummy_app(self()),
  case try_to_fetch_url_or_retry(get, [{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}], 20) of
    {ok, _Headers, Body} ->
      ?assertEqual("Hello World test_app", hd(lists:reverse(Body))),
      % os:cmd(lists:flatten(["kill ", integer_to_list(Bee#bee.os_pid)])),
      passed;
    _ -> 
      ?assertEqual(failed, connect)
  end.
  
start_new_instance_t_failing_app() ->
  bh_test_util:delete_all(app),
  DummyApp = bh_test_util:dummy_app(),
  {error, ErrorObj} = start_dummy_app(
    DummyApp#app{repo_url = "http://this.does/not/exist", 
                 name = "doesnt_exist"}, 
  self()),
  % It should fail when fetching
  ?assertEqual(ErrorObj#app_error.stage, fetching),
  ?assertEqual(ErrorObj#app_error.exit_status, 128),
  passed.
  
teardown_an_instance_t() ->
  % {ok, _App, Bee} = start_dummy_app(self()),
  App = bh_test_util:dummy_app(),
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

start_dummy_app(From) -> start_dummy_app(bh_test_util:dummy_app(), From).
start_dummy_app(App, _From) -> 
  app_manager:request_to_start_new_bee_by_app(App, self()),
  receive
    {bee_started_normally, Bee, _App} ->
      bees:save(Bee),
      {ok, App, Bee};
    {error, ErrorObj} -> 
      {error, ErrorObj};
    X ->
      erlang:display({start_dummy_app, X}),
      X
    after 9000 ->
      erlang:display({timeout})
  end.

try_to_fetch_url_or_retry(_Method, _Args, 0) -> failed;
try_to_fetch_url_or_retry(Method, Args, Times) ->
  case bh_test_util:fetch_url(Method, Args) of
    {ok, _Headers, _Body} = T -> T;
    _E -> try_to_fetch_url_or_retry(Method, Args, Times - 1)
  end.
  
related_dir() -> 
  {ok, Dir} = application:get_env(beehive, beehive_home),
  Dir.
