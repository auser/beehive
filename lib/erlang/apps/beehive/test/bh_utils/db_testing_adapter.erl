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
-export ([init_databases/1, create_tables/0]).

% API
start(Nodes) ->
  add_slave(Nodes),
  ok.

stop() ->
  ok.

% read
read(Table, Key) ->
  case ets:lookup(Table, Key) of
    [B|_Rest] -> get_bee(B);
    _ -> not_found
  end.

write(Table, Key, Record) ->
  case ets:insert(Table, [{Key, Record}]) of
    true -> ok;
    false -> false
  end.

match(Pattern) ->
  Table = element(1, Pattern),
  % Really kind of a hack, but it's okay for the time being, this is just a test database adapter
  get_bee(ets:select(Table, [{{'_', Pattern}, [], ['$_']}])).

save(Fun) when is_function(Fun) ->
  ets:fun2ms(Fun).

delete_all(Table) ->
  ets:delete_all_objects(Table).

delete(Table, Record) ->
  ets:delete(Table, Record).

run(Fun) ->   qlc:eval( Fun() ).

all(Table) -> 
  List = lists:flatten(ets:match(Table, '$1')),
  get_bee(List).
  
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
          % write_concurrency == true so we don't overlap in tests
          ets:new(Tab, [public, named_table, ordered_set, {write_concurrency, true}]), 
          infinite_loop()
        end);
      _ -> ok
    end,
    % Pluralize the table (to match the model module)
    Pluralized = erlang:list_to_atom(lists:append([erlang:atom_to_list(Tab), "s"])),
    code:purge(Pluralized),
    code:load_file(Pluralized),
    case erlang:function_exported(Pluralized, initialize, 0) of
      true -> Pluralized:initialize();
      false -> ok
    end
  end, Databases),
  ok.

infinite_loop() ->
  receive
    _ -> infinite_loop()
  end.

get_bee({_Key, Record}) -> Record;
get_bee(List) -> get_bee(List, []).
get_bee([], Acc) -> lists:reverse(Acc);
get_bee([{_Key, Record}|Rest], Acc) -> get_bee(Rest, [Record|Acc]).
