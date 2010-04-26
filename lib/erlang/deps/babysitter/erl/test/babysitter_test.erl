-module (babysitter_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  babysitter:start_link([]),
  ok.
  
teardown(_X) ->
  babysitter:stop(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_listing_processes/0,
        fun test_starting_one_process/0,
        fun test_exec_one_process/0,
        fun test_starting_many_processes/0,
        fun test_killing_a_process/0,
        fun test_killing_a_process_with_erlang_process/0,
        fun test_status_listing_of_a_process/0,
        % Babysitter fun tests
        fun test_babysitter_config_actions/0
      ]
    }
  }.

test_starting_one_process() ->
  {ok, _ErlProcess, Pid} = babysitter:bs_spawn_run("sleep 2.1", [{env, "HELLO=world"}]),
  CommandArgString = lists:flatten(io_lib:format("ps aux | grep ~p | grep -v grep |awk '{print $2}'", [Pid])),
  ShouldMatch = lists:flatten([erlang:integer_to_list(Pid), "\n"]),
  ?assertCmdOutput(ShouldMatch, CommandArgString).

test_exec_one_process() ->
  {ok, Pid} = babysitter:bs_run("sleep 1.7", [{env, "TEST=true"}]),
  CommandArgString = lists:flatten(io_lib:format("ps aux | grep ~p | grep -v grep |  wc -l | tr -d ' '", [Pid])),
  ?assertCmdOutput("0\n", CommandArgString).

test_starting_many_processes() ->
  Count = 200,
  Fun = fun() -> babysitter:bs_spawn_run("sleep 2.2", [{env, "NAME=$NAME"}, {env, "NAME=bob"}]) end,
  lists:map(fun(_X) -> Fun() end, lists:seq(1, Count)),
  CommandArgString = "ps aux | grep -v grep | grep 'sleep 2.2' | wc -l | tr -d ' '",
  O = ?cmd(CommandArgString),
  {Int, _} = string:to_integer(O),
  ?assertEqual(Int, 200).

test_killing_a_process() ->
  {ok, _ErlProcess, Pid} = babysitter:bs_spawn_run("sleep 2.6", [{env, "NAME=ari"}]),
  {exit_status, Pid, _Status} = babysitter:kill_pid(Pid),
  CommandArgString = lists:flatten(io_lib:format("ps aux | grep ~p | grep -v grep | wc -l | tr -d ' '", [Pid])),
  O = ?cmd(CommandArgString),
  {Int, _} = string:to_integer(O),
  timer:sleep(100),
  ?assertEqual(0, Int).
  
test_killing_a_process_with_erlang_process() ->
  {ok, ErlProcess, Pid} = babysitter:bs_spawn_run("sleep 201.1", [{env, "NAME=ari"}]),
  ErlProcess ! {stop},
  timer:sleep(2),
  CommandArgString = lists:flatten(io_lib:format("ps aux | grep ~p | grep -v grep | wc -l | tr -d ' '", [Pid])),
  O = ?cmd(CommandArgString),
  {Int, _} = string:to_integer(O),
  ?assertEqual(0, Int).

test_listing_processes() ->
  Count = 5,
  Fun = fun() -> babysitter:bs_spawn_run("sleep 2.4", [{env, "NAME=$NAME"}, {env, "NAME=bob"}]) end,
  Pids = lists:map(fun(_X) -> {ok, _ErlProcess, Pid} = Fun(), Pid end, lists:seq(1, Count)),
  {ok, ListOfPids} = babysitter:list(),
  ?assertEqual(ListOfPids, Pids).

test_status_listing_of_a_process() ->
  {ok, _ErlProcess, Pid} = babysitter:bs_spawn_run("sleep 10.8", [{env, "HELLO=world"}]),
  ?assertEqual({status, Pid, 0}, babysitter:status(Pid)),
  babysitter:kill_pid(Pid).

test_babysitter_config_actions() ->
  {ok, _ErlProcess, Pid} = babysitter:bs_spawn_run("sleep 201.1", [{env, "NAME=ari"}]),
  ?assert(true == babysitter:running(Pid)),
  babysitter:kill_pid(Pid).
