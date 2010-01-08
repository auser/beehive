%%%-------------------------------------------------------------------
%%% File    : mesh_util.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Nov 17 14:55:32 PST 2009
%%%-------------------------------------------------------------------

-module (mesh_util).

-export([
         init_db_slave/1,
         add_db_slave/2
        ]).

-export ([
  get_random_pid/1,
  get_best_pid/1
]).

init_db_slave([]) -> ok; % safeguard
init_db_slave(SeedNode) ->
  case (catch(add_db_slave(seed, SeedNode))) of
    {'EXIT', _} ->
      db:stop(),
      mnesia:delete_schema([node()]),
      add_db_slave(seed, SeedNode);
    E -> 
      io:format("E: ~p~n", [E]),
      ok
  end.

% Add an schema node
% Add a slave node FROM the seed node
add_db_slave(slave, SlaveNode) ->
  case (catch mnesia:change_config(extra_db_nodes, [SlaveNode])) of
    {error, _} -> clean_mnesia_from(SlaveNode);
    _ -> ok
  end,
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  Tabs = mnesia:system_info(tables) -- [schema],
  [mnesia:add_table_copy(Tab, SlaveNode, disc_copies) || Tab <- Tabs];
% Add this slave node to the master at SeedNode
add_db_slave(seed, SeedNode) ->
  (catch db:start()),
  mnesia:set_master_nodes([SeedNode]),
  rpc:call(SeedNode, ?MODULE, add_db_slave, [slave, node()]).

clean_mnesia_from(SlaveNode) ->
  rpc:call(SlaveNode, db, stop, []),
  rpc:call(SlaveNode, mnesia, delete_schema, [[SlaveNode]]),
  rpc:call(SlaveNode, db, start, []).


% Strategies for load balancing across the process groups for distributing the 
% load and code handling 
get_random_pid(Name) ->
  L = ensure_get_group_by_name(Name),
  if L == [] ->
      {error, {no_process, Name}};
      true ->
        {_,_,X} = erlang:now(),
        {ok, lists:nth((X rem length(L)) + 1, L)}
  end.

get_best_pid(Name) ->
  L = ensure_get_group_by_name(Name),
  M = lists:map(fun(Pid) ->
    [{message_queue_len, Messages}] = erlang:process_info(Pid, [message_queue_len]),
    {Pid, Messages}
  end, L),
  case lists:keysort(2, M) of
    [{Pid, _} | _] -> Pid;
    [] -> {error, empty_process_group}
  end.
  
% Internal method
ensure_get_group_by_name(Name) ->
  case (catch pg2:get_members(Name)) of
    {'EXIT'} ->
      pg2:create(Name),
      timer:sleep(100),
      pg2:get_members(Name);
    {error, _} ->
      timer:sleep(100),
      pg2:get_members(Name);
    Other when is_list(Other) ->
      Other
    end.