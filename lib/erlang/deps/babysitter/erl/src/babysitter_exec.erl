% 
%  babysitter_exec.erl
%  babysitter
%  
%  Created by Ari Lerner on 2010-04-05.
%  Copyright 2010 Ari Lerner. All rights reserved.
% 
-module (babysitter_exec).

-define (NIF_ERROR_MSG, "NIF Library not laoded").

-on_load(init/0).

-export ([
  test_pid/1,
  test_args/1,
  run_and_monitor/1
]).

init() ->
  Lib = filename:join([
    filename:dirname(code:which(?MODULE)),
    "..", "priv", "lib", ?MODULE
  ]),
  erlang:load_nif(Lib, 0).
  
% A = babysitter_exec:test_args({"hello", [{do_before, "ls"},{do_after, "ls -l"}, {env, "HELLO=world"}, {env, "BOBBY=boy"}, {cd, "/var/babysitter/mine/app/123"}]}). babysitter_exec:test_pid(A).
-spec(test_args/1 :: ({string(), list()}) -> any()).
test_args({_X, _Y}) -> 
  ?NIF_ERROR_MSG.

% A = babysitter_exec:run_and_monitor({"env", [{do_before, "ls"},{do_after, "ls -l"}, {env, "HELLO=world"}, {env, "BOBBY=boy"}, {cd, "/var/babysitter/mine/app/123"}]}). babysitter_exec:test_pid(A).
-spec(run_and_monitor/1 :: ({string(), list()}) -> any()).
run_and_monitor({_Cmd, _Options}) ->
  ?NIF_ERROR_MSG.

% babysitter_exec:test_pid(2).
-spec(test_pid/1 :: (integer()) -> any()).
test_pid(_Pid) ->
  ?NIF_ERROR_MSG.
