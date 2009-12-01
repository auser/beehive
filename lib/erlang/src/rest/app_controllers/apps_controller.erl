%%%-------------------------------------------------------------------
%%% File    : app_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:43 PST 2009
%%%-------------------------------------------------------------------

-module (apps_controller).
-include ("beehive.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(_) -> 
  All = apps:all(),
  {struct, [{
    "apps",
    lists:map(fun(A) ->
      {struct, ?BINIFY([
        {"name", A#app.name},
        {"url", A#app.url},
        {"routing_param", A#app.routing_param},
        {"owner", A#app.routing_param},
        {"last_updated", A#app.updated_at}
      ])}
    end, All)
  }]}.

post(["new"], Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> misc_utils:to_bin("No user defined or invalid token");
    ReqUser ->
      case apps:create(Data) of
        App when is_record(App, app) -> 
          user_apps:create(ReqUser, App),
          {struct, ?BINIFY([{"app", misc_utils:to_bin(App#app.name)}])};
        _ -> "There was an error adding bee\n"
      end
  end;

% Not sure about this... yet as far as authentication goes
post([Name, "restart"], _Data) ->
  Response = case apps:update_by_name(Name) of
    {ok, _} -> {"app", <<"updated">>};
    _ -> {"app", <<"error">>}
  end,
  {struct, ?BINIFY([Response])};
    
post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".