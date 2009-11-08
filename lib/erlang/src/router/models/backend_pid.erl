%%%-------------------------------------------------------------------
%%% File    : backend_pid.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 16:36:24 PST 2009
%%%-------------------------------------------------------------------

-module (backend_pid).
-include ("router.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Backend pids are stored in the database like so:
%   [{<0.1.0>, active, 020203040, "app_name"}]
% |-------------|
% | backendpid  |
% |-------------|
% | backend_name|
% | pid         |
% |-------------|
% 
% We can lookup all the backend pids by looking for the backend key and returning the 
% related array.
% We can lookup the backend associated with a pid by looking through the keys of the 
% pidlist for the Pid and return the backend name, which we do a lookup on the backend
% table to return the backend record
% 
% TODO: Investigate if this is necessary in mnesia, or we can do this in ets

-export ([
  find_backend_for_pid/1,
  find_pids_for_backend_name/1,
  create/1,
  test/0
]).


% Find the backend for the pid given where the pid is in the backend list
% For speed purposes, we'll use dirty read instead of a function
% Even though we have to use 2 functions to lookup the backend, this is still
% faster than using a function. This may have to be placed into a transaction...
find_backend_for_pid(Pid) ->
  [BackendPid|_] = db:read({backend_pid, Pid}),
  [Backend|_] = backend:find_by_name(BackendPid#backend_pid.backend_name),
  Backend.
  
  % db:find(qlc:q([
  %   Backend ||  BackendPid <- mnesia:table(backend_pid),
  %               Pid =:= element(1, BackendPid#backend_pid.pid),
  %               Backend <- mnesia:table(backend),
  %               BackendPid#backend_pid.backend_name =:= Backend#backend.app_name
  % ])).

% Find the pids for the backend named Name
find_pids_for_backend_name(Name) ->
  db:find(qlc:q([
    BackendPid || BackendPid <- mnesia:table(backend_pid),
                  BackendPid#backend_pid.backend_name =:= Name
  ])).
  
% Insert a new backend pid
create(PidTuple) ->
  db:write(PidTuple).


% TESTS
test() ->
  try
    mnesia:clear_table(backend_pid),
    schema:install(),
    Pid = spawn_link(fun() -> forever_loop() end),
    register(pid, Pid),
    Pid2 = spawn_link(fun() -> forever_loop() end),
    register(pid2, Pid2),
    test_insert_backend_pid(),
    find_backend_for_pid_test(),
    find_pids_for_backend_name_test()
  catch
    error: Why ->
      io:format("Test (~p) failed because: ~p", [?MODULE,Why]),
      whereis(pid) ! exit,
      whereis(pid2) ! exit,
      unregister(pid),
      unregister(pid2)
  end.

test_insert_backend_pid() ->
  create(#backend_pid{pid=whereis(pid), backend_name="test_app", status=pending, start_time=63424809235}),
  create(#backend_pid{pid=whereis(pid2), backend_name="another_app", status=active, start_time=63424809236}),
  Fun = fun() -> mnesia:match_object(#backend_pid{_='_'}) end,
  {atomic,Results} = mnesia:transaction(Fun),
  Expect =  [
              #backend_pid{pid=whereis(pid), backend_name="test_app", status=pending, start_time=63424809235},
              #backend_pid{pid=whereis(pid2), backend_name="another_app", status=active, start_time=63424809236}
            ],
  ?assertEqual(lists:sort(Results),Expect).

find_backend_for_pid_test() ->
  Be = #backend{app_name = "test_app"},
  backend:create(Be),
  Res = find_backend_for_pid(whereis(pid)),
  ?assertEqual(Res, Be).
  
find_pids_for_backend_name_test() ->
  create(#backend_pid{pid=whereis(pid), backend_name="test_app", status=active, start_time=63424809235}),
  create(#backend_pid{pid=whereis(pid2), backend_name="another_app", status=pending, start_time=63424809239}),
  Res1 = find_pids_for_backend_name("test_app"),
  Res = lists:map(fun(BP) -> BP#backend_pid.pid end, Res1),
  Expect = [whereis(pid)],
  ?assertEqual(Expect, Res).

% Just to give us pids we can play with
forever_loop() ->
  receive
    exit -> ok;
    _X -> forever_loop()
  end.