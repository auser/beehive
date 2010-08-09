-module (beehive_bee_object_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (DEBUG, false).
-define (CLEANUP (BeeFile, BeeDir, Type), fun() ->
  case ?DEBUG of
    false ->
      case BeeFile of
        undefined -> ok;
        _ -> beehive_bee_object:cleanup(BeeFile)
      end,
      case BeeDir of
        undefined -> ok;
        _ ->  rm_rf(BeeDir)
      end,
      clean_up_dir(Type);
    true -> ok
  end
end()).

-define (DPRINT (Str), fun() ->
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
  ok.

starting_test_() ->
  Tests = {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun git_clone/0
        % fun git_bundle/0,
        % fun git_bundle_with_errors/0,
        % fun responding_from/0,
        % fun ls_bee/0,
        % % Type tests
        % fun bundle_type/0,
        % fun mount_t/0,
        % fun start_t/0,
        % fun stop_t/0,
        % fun cleanup_t/0,
        % fun send_t/0,
        % fun have_bee_t/0
        % fun git_clone_with_errors/0
      ]
    }
  },
  {timeout, 60, Tests}.

git_clone() ->
  ?DPRINT({starting, git_clone}),
  % Update the repos
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
  Ts = lists:flatten(io_lib:format("~w~2..0w~2..0w~2..0w~2..0w~2..0w", [Year, Month, Day, Hour, Minute, Second])),
  
  ReposDir = proplists:get_value(url, git_repos_props()),
  os:cmd([
    "cd ", ReposDir, " && echo '", Ts, "' > LATEST_REV && git commit -a -m 'updated time for testing purposes'"
  ]),
  % Pull one with a specific revision
  Pid = spawn(fun() -> responding_loop([]) end),
  beehive_bee_object:clone([{revision, "281580bc654138df2ca0d7665bc1ac84d4677fc8"}|git_repos_props()], Pid),
  ?assertEqual("281580bc654138df2ca0d7665bc1ac84d4677fc8", get_current_revision(git)),
  
  % Do run it with an after command
  beehive_bee_object:clone([{post, "touch NEW_FILE"}|git_repos_props()]),
  timer:sleep(200), % let it work
  ReposBundleDir = filename:join([related_dir(), "squashed", "testing"]),
  os:cmd(["ls ", ReposBundleDir]),
  ?assert(filelib:is_file(filename:join([ReposBundleDir, "NEW_FILE"]))),
  ?DPRINT({git_clone, passed}),
  passed.

git_bundle() ->
  ?DPRINT({starting, git_bundle}),
  beehive_bee_object:bundle(git_repos_props()),
  BeeFile = filename:join([related_dir(), "squashed", "testing.bee"]),
  erlang:display({bee_file, BeeFile}),
  ?assert(filelib:is_file(BeeFile)),
  % Run it with a before
  beehive_bee_object:bundle([{pre, "touch DUMMY_FILE"}|git_repos_props()]),
  % Untar and ensure the file is there
  BeeDir = filename:join([related_dir(), "squashed", "testing_bee_out"]),
  file:make_dir(BeeDir),
  O = string:tokens(os:cmd(["tar -C ", BeeDir," -zxf ", BeeFile, " && ls ", BeeDir]), "\n"),
  ?assert(lists:member("DUMMY_FILE", O)),
  ?CLEANUP("testing", BeeDir, git),
  ?DPRINT({git_bundle, passed}),
  passed.

% git_clone_with_errors() ->
%   erlang:display({git_clone_with_errors}),
%   % Non-existing url
%   Props = proplists:delete(url, git_repos_props()),
%   clean_up_dir(git),
%   case (catch beehive_bee_object:bundle([{revision, "HEAD"},{url, "http://this.does.not/exist.git"}|Props])) of
%     {'EXIT', _} -> ok;
%     {timeout} -> ok;
%     Out ->
%       ?assertEqual(error, element(1, Out))
%   end,
%   passed.

git_bundle_with_errors() ->
  ?DPRINT({starting, git_bundle_with_errors}),
  ?CLEANUP(undefined, undefined, git),
  ?assertException(
    throw,
    {hook_error, _},
    beehive_bee_object:bundle([{pre, "echo 'ducks'\nexit 1"}|git_repos_props()])),
  ?DPRINT({git_bundle_with_errors, passed}),
  passed.

bundle_type() ->
  ?DPRINT({starting, bundle_type}),
  BeeFile = filename:join([related_dir(), "squashed", "testing.bee"]),
  beehive_bee_object:bundle([{type, rails}|git_repos_props()]),
  % Run it with a before
  % Untar and ensure the file is there
  BeeDir = filename:join([related_dir(), "squashed", "testing_rack_out"]),
  file:make_dir(BeeDir),
  O = string:tokens(os:cmd(["tar -C ", BeeDir," -zxf ", BeeFile, " && ls ", BeeDir]), "\n"),
  ?assert(lists:member("config.ru", O)),
  % Let's make sure beehive_bee_object:info/1 works
  ?assertEqual("master", proplists:get_value(branch, beehive_bee_object:info("testing"))),
  ?assertEqual({error, not_found}, beehive_bee_object:info("no-app-here")),
  ?CLEANUP("testing", BeeDir, git),
  ?DPRINT({bundle_type, passed}),
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
  OldProps = proplists:delete(name, git_repos_props()),
  NewProps = [{name, "crazy_name-045"}|OldProps],
  beehive_bee_object:bundle([{type, rack}|NewProps]),
  BeeDir = filename:join([related_dir(), "squashed"]),
  T = beehive_bee_object:ls(BeeDir),
  ?assert(lists:member("crazy_name-045", T)),
  clean_up_dir(git),
  ?CLEANUP("crazy_name-045", filename:join([BeeDir, "crazy_name-045"]), git),
  ?DPRINT({ls_bee, passed}),
  passed.

mount_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  BeeDir = filename:join([related_dir(), "run"]),
  beehive_bee_object:mount(rack, "testing"),
  ?assert(filelib:is_dir(BeeDir)),
  ?CLEANUP("testing", filename:join([BeeDir, "testing"]), git),
  ?DPRINT({mount_t, passed}),
  passed.

start_t() ->
  Host = "127.0.0.1",
  Port = 9191,
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  beehive_bee_object:mount(rack, "testing"),
  Pid = spawn(fun() -> responding_loop([]) end),
  beehive_bee_object:start(rack, "testing", Port, Pid),
  timer:sleep(500),
  case catch gen_tcp:connect(Host, Port, [binary]) of
    {ok, Sock} -> 
      gen_tcp:close(Sock),
      ?assert(true);
    {error,econnrefused} -> 
      ?assert(false)
  end,
  beehive_bee_object:stop(rack, "testing"),
  ?DPRINT({start_t, passed}),
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
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  beehive_bee_object:mount(rack, "testing"),
  Pid = spawn(fun() -> responding_loop([]) end),
  beehive_bee_object:start(rack, "testing", Port, Pid),
  beehive_bee_object:stop(rack, "testing"),
  timer:sleep(500),
  case catch gen_tcp:connect(Host, Port, [binary]) of
    {ok, Sock} -> 
      gen_tcp:close(Sock),
      ?assert(false);
    {error,econnrefused} -> 
      ?assert(true)
  end,
  ?DPRINT({stop_t, passed}),
  passed.

cleanup_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  Bundle = filename:join([related_dir(), "squashed", "testing.bee"]),
  ?assert(filelib:is_file(Bundle) =:= true),
  beehive_bee_object:cleanup("testing"),
  ?assert(filelib:is_file(Bundle) =:= false),
  ?CLEANUP("testing", Bundle, git),
  passed.
  
send_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  timer:sleep(500),
  BeeObject = beehive_bee_object:get_bee_object(node(self()), "testing"),
  ?assertEqual(git, BeeObject#bee_object.vcs_type),
  ?assertEqual(rack, BeeObject#bee_object.type),
  BeeFile = BeeObject#bee_object.bee_file,
  ?assert(filelib:is_file(BeeFile)),
  ?CLEANUP("testing", undefined, git),
  passed.

have_bee_t() ->
  beehive_bee_object:bundle([{type, rack}|git_repos_props()]),
  ?assert(beehive_bee_object:have_bee("testing") =:= true),
  ?assert(beehive_bee_object:have_bee("weird_app_name") =:= false),
  ?CLEANUP("testing", undefined, git),
  passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TEST HELPERS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
git_repos_props() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  FixtureDir = filename:join([Dir, "test", "fixtures"]),
  ReposDir = filename:join([Dir, "test", "fixtures", "dummy_git"]),
  ReposUrl = lists:concat(["file://", ReposDir]),
  [
    {name, "testing"}, {vcs_type, git}, {url, ReposUrl},
    {fixture_dir, FixtureDir}
  ].

get_current_revision(git) ->
  ReposDir = filename:join([related_dir(), "squashed", "testing"]),
  {ok, OriginalCwd} = file:get_cwd(),
  Rev = os:cmd(lists:flatten(["cd ", ReposDir, " && ", "git rev-parse --verify HEAD^0"])),
  os:cmd(lists:flatten(["cd ", OriginalCwd])),
  string:strip(Rev, right, $\n).

clean_up_dir(git) ->
  % ReposDir = filename:join([related_dir(), "squashed", "testing"]),
  ReposDir = filename:join([related_dir(), "squashed"]),
  c:cd(filename:dirname(ReposDir)),
  rm_rf(ReposDir).

rm_rf(Dir) -> 
  lists:foreach(fun(D) -> rm_rf(D) end, get_dirs(Dir)),
  lists:foreach(fun(File) ->
    file:delete(File)
  end, get_files(Dir)),
  % Now we can remove the empty directory
  file:del_dir(Dir),
  ok.

% Get directories
get_dirs(Dir) -> lists:filter(fun(X) -> filelib:is_dir(X) end, filelib:wildcard(filename:join([Dir, "*"]))).
get_files(Dir) -> lists:filter(fun(X) -> not filelib:is_dir(X) end, filelib:wildcard(filename:join([Dir, "*"]))).

responding_loop(Acc) ->
  receive
    kill -> ok;
    {acc, From} -> 
      From ! {ok, Acc},
      responding_loop(Acc);
    {data, Data} -> 
      % erlang:display({got, Data}),
      responding_loop([Data|Acc])
  end.

related_dir() -> 
  {ok, Dir} = application:get_env(beehive, beehive_home),
  Dir.