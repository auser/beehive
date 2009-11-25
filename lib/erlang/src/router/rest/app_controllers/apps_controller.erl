%%%-------------------------------------------------------------------
%%% File    : app_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:43 PST 2009
%%%-------------------------------------------------------------------

-module (apps_controller).
-include ("router.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(["all"]) -> 
  All = app:all(),
  {struct, [{
    "apps",
    lists:map(fun(A) ->
      {struct, ?BINIFY([
        {"name", A#app.name},
        {"url", A#app.url},
        {"routing_param", A#app.routing_param},
        {"last_updated", A#app.updated_at}
      ])}
    end, All)
  }]};

get(_) -> "hello world".

post([Name], _Data) ->
  case app:update_by_name(Name) of
    {ok, _} -> updated;
    _ -> error
  end;

post(["new"], Data) ->
  case app:create(Data) of
    ok -> {"apps", Data};
    _ -> "There was an error adding bee\n"
  end;
  
post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".