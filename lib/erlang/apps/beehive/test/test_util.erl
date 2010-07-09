-module (test_util).
-author("Ari Lerner <arilerner@mac.com>").
-compile(export_all).

setup_test() ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "fixtures", "beehive.cfg"]),
  application:set_env(beehive, config_file, ConfigFile),
  application:set_env(beehive, beehive_home, "/tmp/beehive/test"),
  application:set_env(beehive, database_dir, "/tmp/beehive/test/test_db"),
  
  beehive:start(),
  ok.
  
setup_test(Table) ->
  % beehive_db_srv:start_link(),
  % application:start(sasl),
  setup_test(),
  delete_all(Table),
  ok.

teardown_test() ->
  application:set_env(beehive, beehive_home, "/tmp/beehive/test"),
  beehive:stop(),
  erlang:display({reminder, remove, beehive_home, "/tmp/beehive/test"}),
  ok.
  
delete_all(Table) ->
  lists:map(fun(Obj) ->
    beehive_db_srv:delete(Table, Obj)
  end, beehive_db_srv:all(Table)),
  ok.

start(Count)      -> start(Count, example_cluster_srv, 0, []).
start(Count, Mod) -> start(Count, Mod, 0, []).
start(Count, _Mod, Count, Acc) -> {ok, Acc};
start(Count, Mod, CurrentCount, Acc) ->
  Name = erlang:list_to_atom(lists:flatten(["node", erlang:integer_to_list(CurrentCount)])),
  Seed = case erlang:length(Acc) of
    0 -> undefined;
    _ -> whereis(erlang:hd(Acc))
  end,
  {ok, _NodePid} = Mod:start_named(Name, [{seed, Seed}]),
  start(Count, Mod, CurrentCount + 1, [Name|Acc]).
  
shutdown([]) -> ok;
shutdown([Pname|Rest]) -> 
  Pid = whereis(Pname),
  gen_cluster:cast(Pid, stop), 
  try unregister(Pname)
  catch _:_ -> ok
  end,
  shutdown(Rest).

context_run(Count, Fun) ->
  {ok, Nodes} = start(Count),
  Fun(),
  shutdown(Nodes).