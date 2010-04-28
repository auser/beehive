%%%-------------------------------------------------------------------
%%% File    : mesh_util.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Nov 17 14:55:32 PST 2009
%%%-------------------------------------------------------------------

-module (mesh_util).

-export ([
  get_random_pid/1,
  get_best_pid/1
]).

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
    {error, Reason} -> [];
    Other when is_list(Other) ->
      Other
    end.