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
-include_lib("stdlib/include/qlc.hrl").

-export ([
  find_by_name/1,
  get/1,
  create/1,
  new/1,
  update/1,
  delete/1,
  all/0
]).

find_by_name(Hostname) ->
  db:read({backend, Hostname}).

new(NewBackend) when is_record(NewBackend, backend) ->
  NewBackend;

new(NewProps) ->
  PropList = ?rec_info(backend, #backend{}),
  FilteredProplist = misc_utils:filter_proplist(PropList, NewProps, []),
  list_to_tuple([backend|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, backend)]]).

get(_) ->
  ok.
  
create(Backend) when is_record(Backend, backend) ->
  db:write(Backend);
create(Proplist) ->
  db:write(new(Proplist)).

update(_) ->
  ok.

delete(_) ->
  ok.

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(backend) ])).
  
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
