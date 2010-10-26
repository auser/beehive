-module (db_mnesia_adapter).

-include ("beehive.hrl").
-include ("common.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export ([
  start/1,
  stop/0,
  read/2,
  save/1,
  write/3,
  run/1,
  delete/2, delete_all/1,
  all/1,
  match/1,
  info/1
]).

-export ([init_databases/0, init_databases/1]).

%% API
info(Type) ->
  mnesia:system_info(Type).

start(Nodes) ->
  init_databases(Nodes).

stop() ->
  application:stop(mnesia),
  ok.

%% read
read(Table, Key) ->
  {_Time, Value} = timer:tc(mnesia, dirty_read, [{Table, Key}]),
  Value.

write(_Table, _Key, Record) ->
  {_Time, Value} = timer:tc(mnesia, dirty_write, [Record]),
  Value.
  %% case mnesia:transaction(fun() -> mnesia:write(Record) end) of
  %%   {aborted, Reason} -> {error, Reason};
  %%   {atomic, _} -> ok
  %% end.

match(Pattern) ->
  {_Time, Value} = timer:tc(mnesia, dirty_match_object, [Pattern]),
  Value.

save(Fun) ->
  case mnesia:transaction(Fun) of
    {aborted, Reason} -> {error, Reason};
    {atomic, _} -> ok
  end.

delete_all(Table) ->
  mnesia:clear_table(Table).

delete(Table, Record) ->
  case mnesia:transaction(fun() -> mnesia:delete({Table, Record}) end) of
    {aborted, Reason} -> {error, Reason};
    {atomic, _} -> ok
  end.

run(Fun) ->   qlc:eval( Fun() ).

all(Table) ->
  Fun = fun() ->
    Query = qlc:q([ Ele || Ele <- mnesia:table(Table) ]),
    qlc:e(Query)
  end,
  case mnesia:transaction(Fun) of
    {aborted, Reason} -> {error, Reason};
    {atomic, V} -> V
  end.


%%%% INTERNAL
init_databases() -> init_databases([node(self())]).
init_databases(Nodes) ->
  ok = ensure_dir(),
  ok = ensure_running(),
  ok = add_slave(Nodes),
  ok = wait_for_tables(),
  %% Add default data
  ok.

dir() ->
  DefaultDatabaseDir = config:search_for_application_value(database_dir,
                                                           ?BEEHIVE_DIR("db")),
  case application:get_env(mnesia, dir) of
    {ok, Dir} -> Dir;
    _Else ->
      application:set_env(mnesia, dir, DefaultDatabaseDir),
      DefaultDatabaseDir
  end.
  %% mnesia:system_info(directory).

ensure_running() ->
  case mnesia:system_info(is_running) of
    yes -> ok;
    no -> mnesia:start()
  end.

ensure_not_running() ->
  case mnesia:system_info(is_running) of
    yes -> mnesia:stop();
    no -> ok
  end.

ensure_dir() ->
  case bh_file_utils:ensure_dir_exists([dir()]) of
    {error, Reason} -> throw({error, Reason});
    ok -> ok
  end.

%% Thanks to RabbitMQ for the idea
add_slave(Nodes) ->
  case mnesia:change_config(extra_db_nodes, Nodes -- [node()]) of
    {ok, []} ->
      case mnesia:system_info(use_dir) of
        true ->
          case check_schema_integrity() of
            ok -> ok;
            {error, Reason} ->
              ?LOG(error, "Schema integrity check failed: ~p", [Reason]),
              ok = move_db(),
              ok = create_schema()
          end;
        false -> create_schema()
      end;
    {ok, [_|_]} ->
      Type = case (Nodes == [] orelse lists:member(node(), Nodes)) of
        true -> disc;
        false -> ram
      end,
      ok = wait_for_replicated_tables(),
      ok = create_local_table_copy(schema, disc_copies),
      ok = create_local_table_copies(Type);
    {error, Reason} ->
      throw({error, {unable_to_join_db_cluster, Nodes, Reason}})
  end.

%% Create the databases
create_schema() ->
  ensure_not_running(),
  mnesia:create_schema([node()]),
  ensure_running(),
  create_tables().

%% Move the database directory to the backup directory and then try to
%% create it to start fresh-like
move_db() ->
  mnesia:stop(),
  MnesiaDir = filename:dirname(dir() ++ "/"),
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
  BackupDir =
    lists:flatten(io_lib:format("~s_~w~2..0w~2..0w~2..0w~2..0w~2..0w",
                                [MnesiaDir, Year, Month, Day, Hour,
                                 Minute, Second])),
  case file:rename(MnesiaDir, BackupDir) of
    ok ->
      error_logger:warning_msg("moved database from ~s to ~s~n",
                               [MnesiaDir, BackupDir]),
      ok;
    {error, Reason} ->
      throw({error, {cannot_backup_mnesia, MnesiaDir, BackupDir, Reason}})
  end,
  ok = ensure_dir(),
  ok = ensure_running(),
  ok.

table_names() ->
  [Tab || {Tab, _} <- table_definitions()].

table_definitions() ->
  [
    {app,
     [{attributes, record_info(fields, app)},
      {type, set},
      {disc_copies, [node()]}]},
    {bee,
     [{attributes, record_info(fields, bee)},
      {type, set},
      {disc_copies, [node()]}]},
    {user,
     [{attributes, record_info(fields, user)},
      {type, set},
      {disc_copies, [node()]}]},
    {user_app,
     [{attributes, record_info(fields, user_app)},
      {type, set},
      {disc_copies, [node()]}]}
  ].

%% Create the tables
create_tables() ->
  Databases = table_definitions(),
  lists:foreach(fun({Tab, TabAttr}) ->
    case mnesia:create_table(Tab, TabAttr) of
      {atomic, ok} -> ok;
      {aborted, _Reason} -> ok %throw({error, {table_creation_failed, Tab, TabAttr, Reason}})
    end,
    %% Pluralize the table (to match the model module)
    Pluralized =
        erlang:list_to_atom(lists:append([erlang:atom_to_list(Tab), "s"])),
    code:load_file(Pluralized),
    erlang:display({Pluralized,initialize,0,
                    erlang:function_exported(Pluralized, initialize, 0)}),
    case erlang:function_exported(Pluralized, initialize, 0) of
      true -> Pluralized:initialize();
      false -> ok
    end
  end, Databases),
  ok.

%% Create local table copies
create_local_table_copies(Type) ->
  lists:foreach(
  fun({Tab, TabDef}) ->
    HasDiscCopies     = table_has_copy_type(TabDef, disc_copies),
    HasDiscOnlyCopies = table_has_copy_type(TabDef, disc_only_copies),
    StorageType =
    if
      Type =:= disc ->
        if
          HasDiscCopies     -> disc_copies;
          HasDiscOnlyCopies -> disc_only_copies;
          true              -> ram_copies
        end;
        Type =:= ram -> ram_copies
      end,
      ok = create_local_table_copy(Tab, StorageType)
    end,
    table_definitions()),
  ok.

table_has_copy_type(TabDef, DiscType) -> lists:member(node(), proplists:get_value(DiscType, TabDef, [])).

%% Create a copy of the Table locally
create_local_table_copy(Table, Type) ->
  StorageType = mnesia:table_info(Table, storage_type),
  {atomic, ok} =
    if
      StorageType == unknown ->
        mnesia:add_table_copy(Table, node(), Type);
      StorageType /= Type ->
        mnesia:change_table_copy_type(Table, node, Type);
      true -> {atomic, ok}
    end,
  ok.

%% Wait for the tables
wait_for_replicated_tables() -> wait_for_tables(table_names()).
wait_for_tables() -> wait_for_tables(table_names()).
wait_for_tables(TableNames) ->
  case check_schema_integrity() of
    ok ->
      case catch mnesia:wait_for_tables(TableNames, 5000) of
        ok -> ok;
        {timeout, _BadTabs} ->
          error_logger:warning_msg(
            "Could not wait for the tables to be ready: ~p~n"
            "moving database to backup location "
            "and recreating schema from scratch~n",
            [timeout]),
          ok = move_db(),
          ok = create_schema();
          %% throw({error, {timeout_waiting_for_tables, BadTabs}});
        {error, Reason} -> throw({error, {failed_waiting_for_tables, Reason}})
      end;
    {error, Reason} ->
      throw({error, {schema_integrity_check_failed, Reason}})
  end.

check_schema_integrity() ->
  case catch [mnesia:table_info(Tab, version) || Tab <- table_names()] of
    {'EXIT', Reason} -> {error, Reason};
    _ -> ok
  end.
