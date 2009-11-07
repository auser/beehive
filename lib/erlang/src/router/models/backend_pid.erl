%%%-------------------------------------------------------------------
%%% File    : backend_pid.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 16:36:24 PST 2009
%%%-------------------------------------------------------------------

-module (backend_pid).
-include ("router.hrl").

% Backend pids are stored in the database like so:
%   [{BackendName, [{<0.1.0>, active, 020203040}, {<0.2.0>, pending, 020203041}]}]
% |-------------|
% | backendpid  |
% |-------------|
% | backend_key |
% | pid         |
% |-------------|
% 
% We can lookup all the backend pids by looking for the backend key and returning the 
% related array.
% We can lookup the backend associated with a pid by looking through the keys of the 
% pidlist for the Pid and return the backend name, which we do a lookup on the backend
% table to return the backend record

-export ([
  find_backend_for_pid/1,
  find_pids_for_backend_name/1
]).


% Find the backend for the pid given where the pid is in the backend list
find_backend_for_pid(Pid) ->
  db:find([
    Backend ||  BackendPid <- mnesia:table(backend_pid),
                true =:= lists:keymember(Pid, 1, BackendPid#backend_pid.pids),
                Backend <- mnesia:table(backend),
                BackendPid#backend_pid.backend_key =:= Backend#backend.app_name
  ]).

% Find the pids for the backend named Name
find_pids_for_backend_name(Name) ->
  db:find([
    BackendPid#backend_pid.pids ||  BackendPid <- mnesia:table(backend_pid),
                                    BackendPid#backend_pid.backend_key =:= Name
  ]).