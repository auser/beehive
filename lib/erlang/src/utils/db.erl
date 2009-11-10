%%%-------------------------------------------------------------------
%%% File    : db.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:31:12 PST 2009
%%%-------------------------------------------------------------------

-module (db).

-include ("router.hrl").

-export ([
  init/0,
  write/1,
  delete/2,
  delete_object/1,
  read/1,
  index_read/3,
  find/1,
  clear_table/1,
  new_id/1,
  match/1,
  transaction/1
]).

init() -> schema:install().

new_id(Key) ->
	mnesia:dirty_update_counter({counter, Key}, 1).
	
write(Record) ->
  {_Time, Value} = timer:tc(mnesia, dirty_write, [Record]),
  Value.
  
delete(Table, Key) ->
  {_Time, Value} = timer:tc(mnesia, dirty_delete, [Table, Key]),
  Value.

delete_object(Pattern) ->
  delete_objects(match(Pattern)).
  
delete_objects(Objects) ->
  transaction(fun() -> lists:foreach(fun mnesia:delete_object/1, Objects) end).

find(F) when is_function(F) ->
  {_Time, Value} = timer:tc(?MODULE, transaction, [F]),
  Value;

find(Q) ->
  F = fun() -> qlc:eval(Q) end,
  {_Time, Value} = timer:tc(?MODULE, transaction, [F]),
  Value.

read(Tuple) ->
  {_Time, Value} = timer:tc(mnesia, dirty_read, [Tuple]),
  Value.
  
match(Pattern) ->
  {_Time, Value} = timer:tc(mnesia, dirty_match_object, [Pattern]),
  Value.

index_read(Table, Value, Key) ->
  {_Time, Value} = timer:tc(mnesia, dirty_index_read, [Table, Value, Key]),
  Value.

clear_table(Table) ->
  mnesia:clear_table(Table).

transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, _Reason} ->
			[]
	end.