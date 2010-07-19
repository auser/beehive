-module (db_testing_adapter).

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
  match/1
]).

% For RPC calls
-export ([init_databases/1]).

% API
start(Nodes) ->
  add_slave(Nodes),
  ok.

stop() ->
  ok.

% read
read(Table, Key) ->
  case ets:lookup(Table, Key) of
    [B|_Rest] -> B;
    _ -> not_found
  end.

write(Table, Key, Record) ->
  case ets:insert(Table, {Key, Record}) of
    true -> ok;
    false -> false
  end.

match(Pattern) ->
  Table = element(1, Pattern),
  ets:match(Table, Pattern).

save(Fun) ->
  ets:fun2ms(Fun).

delete_all(Table) ->
  ets:delete_all_objects(Table).

delete(Table, Record) ->
  ets:delete(Table, Record).

run(Fun) ->   qlc:eval( Fun() ).

all(Table) -> 
  ets:match(Table, '$1').
  
% Thanks to RabbitMQ for the idea
add_slave([]) -> create_tables();
add_slave(Nodes) ->
  lists:map(fun(Node) ->
    rpc:call(Node, ?MODULE, create_tables, [])
  end, Nodes).

table_definitions() ->
  [app, bee, user, user_app].

init_databases(Nodes) ->
  add_slave(Nodes).

% Create the tables
create_tables() ->
  Databases = table_definitions(),  
  lists:foreach(fun(Tab) ->
    case ets:info(Tab) of
      undefined -> 
        spawn(fun() -> 
          ets:new(Tab, [public, named_table, set]), 
          infinite_loop()
        end);
      _ -> ok
    end,
    % Pluralize the table (to match the model module)
    Pluralized = erlang:list_to_atom(lists:append([erlang:atom_to_list(Tab), "s"])),
    case code:load_file(Pluralized) of
      {error, _} = T -> throw(T);
      _ ->
        case erlang:function_exported(Pluralized, initialize, 0) of
          true -> Pluralized:initialize();
          false -> ok
        end
    end
  end, Databases),
  ok.

infinite_loop() ->
  receive
    _ -> infinite_loop()
  end.