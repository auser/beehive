-module (beehive_bee_object_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (DEBUG, false).
-define (CLEANUP, fun() ->
  case ?DEBUG of
    false ->
      lists:map(fun(Dir) -> clean_up_dir(Dir) end, ["squashed", "run"]);
    true -> ok
  end
end()).

-define (DEBUG_PRINT (Str), fun() ->
  case ?DEBUG of
    true -> erlang:display(Str);
    false -> ok
  end
end()).

setup() ->
  bh_test_util:setup([]),
  beehive_bee_object:init(),
  ok.

teardown(_X) ->
  ?CLEANUP,
  ok.

all_test_() ->
  Tests = {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun git_clone/0
        ,fun git_bundle/0
        ,fun git_bundle_with_errors/0
        ,fun responding_from/0
        ,fun ls_bee/0
        % Type tests
        ,fun bundle_template/0
        ,fun mount_t/0
        ,fun start_t/0
        ,fun stop_t/0
        ,fun cleanup_t/0
        ,fun send_t/0
        ,fun have_bee_t/0
        ,fun start_bee_with_no_object_in_memory/0
        % ,fun git_clone_with_errors/0
      ]
    }
  },
  {timeout, 90, Tests}.

git_clone() ->
  ?DEBUG_PRINT({starting, git_clone}),
  % Update the repos
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
  Ts = lists:flatten(io_lib:format("~w~2..0w~2..0w~2..0w~2..0w~2..0w",
                                   [Year, Month, Day, Hour, Minute, Second])),

  ReposDir = proplists:get_value(repo_url, git_repos_props()),

  os:cmd([
    "cd ", ReposDir, " && echo '", Ts, "' > LATEST_REV && git commit -a -m 'updated time for beehive_bee_object_test_app purposes'"
  ]),
  % Pull one with a specific revision
  Rev = "7b6221ef298d26167e4ba5da13e55b9af57274e7",
  Pid = spawn(fun() -> responding_loop([]) end),
  beehive_bee_object:clone([{revision, Rev}|git_repos_props()], Pid),
  timer:sleep(500),
  ?assertEqual(Rev, get_current_revision(git)),

  % Do run it with an after command
  beehive_bee_object:clone([{post, "touch NEW_FILE"}|git_repos_props()]),
  timer:sleep(200), % let it work
  ReposBundleDir = filename:join([related_dir(), "squashed", "beehive_bee_object_test_app"]),
  os:cmd(["ls ", ReposBundleDir]),
  ?assert(filelib:is_file(filename:join([ReposBundleDir, "NEW_FILE"]))),
  ?DEBUG_PRINT({git_clone, passed}),
  passed.

git_bundle() ->
  rm_rf(filename:join([related_dir(), "squashed", "testing_bee_out"])),
  ?DEBUG_PRINT({starting, git_bundle}),
  beehive_bee_object:bundle(git_repos_props()),
  BeeFile = filename:join([related_dir(), "squashed", "beehive_bee_object_test_app.bee"]),
  ?assert(filelib:is_file(BeeFile)),
  % Run it with a before
  beehive_bee_object:bundle([{pre, "touch DUMMY_FILE"}|git_repos_props()]),
  % Untar and ensure the file is there
  BeeDir = filename:join([related_dir(), "squashed", "testing_bee_out"]),
  file:make_dir(BeeDir),
  O = string:tokens(os:cmd(["tar -C ", BeeDir," -zxf ", BeeFile, " && ls ", BeeDir]), "\n"),
  ?assert(lists:member("DUMMY_FILE", O)),
  rm_rf(BeeDir),
  ?DEBUG_PRINT({git_bundle, passed}),
  passed.

% git_clone_with_errors() ->
%   % Non-existing url
%   ?DEBUG_PRINT({git_clone_with_errors}),
%   Props1 = proplists:delete(url, git_repos_props()),
%   Props  = proplists:delete(name, Props1),
%   Pid = spawn(fun() -> responding_loop([]) end),
%   case (catch beehive_bee_object:bundle([{name, "error_clone"},{url, "http://this.does.not/exist.git"}|Props], Pid)) of
%     {'EXIT', _} -> ok;
%     {timeout} -> ok;
%     Out ->
%       ?assertEqual(error, element(1, Out))
%   end,
%   passed.

git_bundle_with_errors() ->
  ?DEBUG_PRINT({starting, git_bundle_with_errors}),
  ?assertException(
    throw,
    {hook_error, _},
    beehive_bee_object:bundle([{pre, "echo 'ducks'\nexit 1"}|git_repos_props()])),
  ?DEBUG_PRINT({git_bundle_with_errors, passed}),
  passed.

bundle_template() ->
  ?DEBUG_PRINT({starting, bundle_template}),
  BeeFile = filename:join([related_dir(), "squashed", "beehive_bee_object_test_app.bee"]),
  beehive_bee_object:bundle([{template, rails}|git_repos_props()]),
  % Run it with a before
  % Untar and ensure the file is there
  BeeDir = filename:join([related_dir(), "squashed", "testing_rack_out"]),
  file:make_dir(BeeDir),
  O = string:tokens(
        os:cmd(["tar -C ", BeeDir," -zxf ", BeeFile, " && ls ", BeeDir]), "\n"),
  ?assert(lists:member("config.ru", O)),
  % Let's make sure beehive_bee_object:info/1 works
  ?assertEqual("master",
               proplists:get_value(branch,
                                   beehive_bee_object:info("beehive_bee_object_test_app"))),
  ?assertEqual({error, not_found}, beehive_bee_object:info("no-app-here")),
  ?DEBUG_PRINT({bundle_template, passed}),
  passed.

responding_from() ->
  Pid = spawn(fun() -> responding_loop([]) end),
  beehive_bee_object:bundle([{type, rails}|git_repos_props()], Pid),
  timer:sleep(500),
  Pid ! {acc, self()},
  O1 = receive
    {ok, Data} -> Data
    after 50 -> []
  end,
  ?assert(erlang:length(O1) =/= 0),
  Pid ! kill,
  passed.

ls_bee() ->
  ReposUrl = bh_test_util:dummy_git_repos_url(),

  NewProps = [{name, "crazy_name-045"},{repo_url, ReposUrl},{repo_type, git}],
  beehive_bee_object:bundle([{type, python}|NewProps], self()),
  F = fun(This) ->
    receive
      {data, Data} ->
        ?DEBUG_PRINT({got, data, Data}),
        This(This);
      {port_closed, _} -> This(This);
      {error, Reason} ->
        erlang:display({error, Reason});
      X ->
        erlang:display({bundling,got,X})
      after 1000 -> ok
    end
  end,
  F(F),
  T = beehive_bee_object:ls(),
  ?assert(lists:member("crazy_name-045", T)),
  ?DEBUG_PRINT({ls_bee, passed}),
  passed.

mount_t() ->
  Params = [{type, rack},{deploy_env, "staging"}|git_repos_props()],
  beehive_bee_object:bundle(Params),
  BeeDir = filename:join([related_dir(), "run"]),
  beehive_bee_object:mount(apps:new(Params)),
  ?assert(filelib:is_dir(BeeDir)),
  ?DEBUG_PRINT({mount_t, passed}),
  [{Name, Bob}|_Rest] =
    ets:lookup('beehive_bee_object_info',
               proplists:get_value(name, Params)),
  ProplistBob = dict:to_list(Bob),
  ?assertEqual("staging", proplists:get_value(deploy_env, ProplistBob)),
  passed.

start_t() ->
  Host = "127.0.0.1",
  Port = 9192,
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  Pid = spawn(fun() -> responding_loop([]) end),
  {started, BeeObject} = beehive_bee_object:start(#app{template=rack, name="beehive_bee_object_test_app"}, Port, Pid),
  timer:sleep(600),
  case catch gen_tcp:connect(Host, Port, [binary]) of
    {ok, Sock} ->
      gen_tcp:close(Sock),
      beehive_bee_object:stop(BeeObject),
      ?assert(true);
    {error,econnrefused} ->
      ?assert(false)
  end,
  ?DEBUG_PRINT({start_t, passed}),
  % case bh_test_util:try_to_fetch_url_or_retry(get, [{host, "127.0.0.1"}, {port, Port}, {path, "/"}], 20) of
  %   {ok, _Headers, Body} ->
  %     ?assertEqual("Hello world", Body),
  %     passed;
  %   _ ->
  %     ?assertEqual(failed, connect)
  % end,
  passed.

stop_t() ->
  Host = "127.0.0.1",
  Port = 9191,
  ReposUrl = bh_test_util:dummy_git_repos_url(),
  Name = "app_intended_to_test_stopping",
  NewProps = [{name, Name},{repo_url, ReposUrl},{repo_type, git},{type, rack},{fixture_dir, fixture_dir()}],
  Pid = spawn(fun() -> responding_loop([]) end),
  beehive_bee_object:bundle(NewProps, Pid),
  {started, BeeObject} = beehive_bee_object:start(#app{template=rack,name= Name}, Port, Pid),
  timer:sleep(100),
  Q = beehive_bee_object:stop(Name, Pid),
  ?DEBUG_PRINT({beehive_bee_object,stop,Q,BeeObject#bee_object.pid,Name,Pid}),
  timer:sleep(500),
  GenTcpOut = gen_tcp:connect(Host, Port, [binary]),
  ?assertMatch({ok, _},  GenTcpOut),
  gen_tcp:close(element(2, GenTcpOut)),
  beehive_bee_object:stop(BeeObject, Pid),
  ?DEBUG_PRINT({stop_t, passed}),
  passed.

cleanup_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  Bundle = filename:join([related_dir(), "squashed", "beehive_bee_object_test_app.bee"]),
  ?assert(filelib:is_file(Bundle) =:= true),
  beehive_bee_object:cleanup("beehive_bee_object_test_app"),
  ?assert(filelib:is_file(filename:join([related_dir(), "run", "beehive_bee_object_test_app"])) =:= false),
  passed.

send_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  timer:sleep(500),
  BeeObject = beehive_bee_object:get_bee_object(node(self()), "beehive_bee_object_test_app"),
  ?assertEqual(git, BeeObject#bee_object.repo_type),
  ?assertEqual(rack, BeeObject#bee_object.template),
  BeeFile = BeeObject#bee_object.bee_file,
  ?assert(filelib:is_file(BeeFile)),
  passed.

have_bee_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  ?assert(beehive_bee_object:have_bee("beehive_bee_object_test_app") =:= true),
  ?assert(beehive_bee_object:have_bee("weird_app_name") =:= false),
  passed.

start_bee_with_no_object_in_memory() ->
  DummyApp = bh_test_util:dummy_app("nonprod_app"),
  DummyApp1 = DummyApp#app{deploy_env = "nonprod"},
  apps:save(DummyApp1),
  App = apps:find_by_name(DummyApp1#app.name),
  beehive_bee_object:bundle(apps:to_proplist(App)),

  %% Actually deleting the reference from the bobject store
  ets:delete('beehive_bee_object_info', App#app.name),

  Pid = spawn(fun() -> responding_loop([]) end),
  Foo = beehive_bee_object:start(App, 9876, Pid),
  passed.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TEST HELPERS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
git_repos_props() ->
  ReposUrl = bh_test_util:dummy_git_repos_url(),
  [
    {name, "beehive_bee_object_test_app"}, {repo_type, git}, {repo_url, ReposUrl},
    {fixture_dir, fixture_dir()}
  ].

get_current_revision(git) ->
  ReposDir = filename:join([related_dir(), "squashed", "beehive_bee_object_test_app"]),
  {ok, OriginalCwd} = file:get_cwd(),
  Rev = os:cmd(lists:flatten(["cd ", ReposDir, " && ", "git rev-parse --verify HEAD^0"])),
  os:cmd(lists:flatten(["cd ", OriginalCwd])),
  string:strip(Rev, right, $\n).

clean_up_dir(Dir) ->
  rm_rf(filename:join([related_dir(),Dir])).

rm_rf(Dir) ->
  bh_file_utils:rm_rf(Dir).

fixture_dir() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  filename:join([Dir, "test", "fixtures"]).

responding_loop(Acc) ->
  receive
    kill -> ok;
    {acc, From} ->
      From ! {ok, Acc},
      responding_loop(Acc);
    {data, Data} ->
      ?DEBUG_PRINT({got,responding_loop,Data}),
      responding_loop([Data|Acc])
  end.

related_dir() ->
  {ok, Dir} = application:get_env(beehive, beehive_home),
  Dir.
