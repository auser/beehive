%%%-------------------------------------------------------------------
%%% File    : backend.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 13:15:07 PST 2009
%%%-------------------------------------------------------------------

-module (backend).

-include ("router.hrl").

-export ([
  find_by_hostname/1,
  create_or_update/1,
  get/1,
  create/1,
  update/1,
  delete/1
]).

find_by_hostname(Hostname) ->
  db:read({backend, Hostname}).

create_or_update(NewBackend) ->
  ok.

new(NewProps) ->
  #backend{}.
%   PropList = ?rec_info(backend, Backend),
%   FilteredProplist = filter_backend_proplist(PropList, NewProps, []),
%   list_to_tuple([backend|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, backend)]]).
%   
% % Only choose values that are actually in the proplist
% filter_backend_proplist(_BackendProplist, [], Acc) -> Acc;
% filter_backend_proplist(BackendProplist, [{K,V}|Rest], Acc) ->
%   case proplists:is_defined(K, BackendProplist) of
%     false -> filter_backend_proplist(BackendProplist, Rest, Acc);
%     true -> filter_backend_proplist(BackendProplist, Rest, [{K,V}|Acc])
%   end.

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
  	B#backend.name == "" -> false;
  	B#backend.port =< 0 -> false;
  	B#backend.maxconn =< 0 -> false;
  	B#backend.act_count =/= 0 -> false;
  	B#backend.act_time =/= 0 -> false;
  	true -> true
  end.
