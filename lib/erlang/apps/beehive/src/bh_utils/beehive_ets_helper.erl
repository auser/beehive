-module (beehive_ets_helper).

-export ([spawn_and_monitor/2]).
% Internal
-export ([ets_process_restarter/2, start_ets_process/1]).

spawn_and_monitor(Name, TableNames) ->
  spawn(?MODULE, ets_process_restarter, [Name, TableNames]).

ets_process_restarter(Name, TableNames) ->
  process_flag(trap_exit, true),
  start_ets_processes(Name, TableNames).
  % Pid = spawn_link(?MODULE, start_ets_process, [Name]),
  % catch register(Name, Pid),
  % receive
  %   {'EXIT', Pid, normal} -> % not a crash
  %     ok;
  %   {'EXIT', Pid, shutdown} -> % manual termination, not a crash
  %     ok;
  %   {'EXIT', Pid, _} ->
  %     unregister(Name),
  %     ets_process_restarter(Name)
  % end.

start_ets_processes(EtsTableName, TableNames) ->
  lists:map(fun(Name) ->
    spawn(fun() ->
      Pid = spawn_link(?MODULE, start_ets_process, [Name]),
      catch register(EtsTableName, Pid),
      receive
        {'EXIT', Pid, normal} -> % not a crash
          ok;
        {'EXIT', Pid, shutdown} -> % manual termination, not a crash
          ok;
        {'EXIT', Pid, _} ->
          unregister(EtsTableName),
          ets_process_restarter(EtsTableName, TableNames)
      end
    end)
  end, TableNames).

start_ets_process(Name) ->
  TableOpts = [set, named_table, public],
  case catch ets:info(Name) of
    undefined -> ets:new(Name, TableOpts);
    _ -> ok
  end,
  receive
    kill -> ok;
    _ -> start_ets_process(Name)
  end.
