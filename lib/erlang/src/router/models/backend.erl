%%%-------------------------------------------------------------------
%%% File    : backend.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 13:15:07 PST 2009
%%%-------------------------------------------------------------------

-module (backend).

% This provides an interface to the backend mnesia table that looks like:
% |----------|
% | backends |
% |----------|
% | name     |
% | host     |
% | ...      |
% |----------|
% 

-include ("router.hrl").

-export ([
  find_by_hostname/1,
  create_or_update/1,
  get/1,
  create/1,
  new/1,
  update/1,
  delete/1
]).

find_by_hostname(Hostname) ->
  db:read({backend, Hostname}).

create_or_update(_NewBackend) ->
  ok.

new(NewBackend) when is_record(NewBackend, backend) ->
  NewBackend;

new(NewProps) ->
  PropList = ?rec_info(backend, #backend{}),
  FilteredProplist = misc_utils:filter_proplist(PropList, NewProps, []),
  list_to_tuple([backend|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, backend)]]).

get(_) ->
  ok.
  
create(_) ->
  ok.

update(_) ->
  ok.

delete(_) ->
  ok.
  
% Check to make sure the backend is valid
valid(B) ->
  Real = #backend{},
  if 
	  size(B) =/= size(Real) -> false;
  	B#backend.app_name == "" -> false;
  	B#backend.port =< 0 -> false;
  	B#backend.maxconn =< 0 -> false;
  	B#backend.act_count =/= 0 -> false;
  	B#backend.act_time =/= 0 -> false;
  	true -> true
  end.
