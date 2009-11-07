%%%-------------------------------------------------------------------
%%% File    : app_backends.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 15:46:20 PST 2009
%%%-------------------------------------------------------------------

-module (app_backends).

-export ([
  find_by_hostname/1,
  get/1,
  create/1,
  update/1,
  delete/1
]).

find_by_hostname(Name) ->
  db:read({app, Name}).

get(Name) ->
  case db:read({app, Name}) of
    [App] -> App;
    [] -> []
  end.
  
create(_) ->
  ok.

update(_) ->
  ok.

delete(_) ->
  ok.
