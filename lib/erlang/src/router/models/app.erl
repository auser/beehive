%%%-------------------------------------------------------------------
%%% File    : app.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:22:05 PST 2009
%%%-------------------------------------------------------------------

-module (app).
-include ("router.hrl").

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
  
% Insert a new app
create(App) when is_record(App, app) ->
  db:write(App).

update(_) ->
  ok.

delete(_) ->
  ok.
  