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
      Details = compile_app_details(A),
      {struct, ?BINIFY(Details)}
    end, All)
  }]}.

post(["new"], Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> 
      {struct, [{"error", misc_utils:to_bin("No user defined or invalid token")}]};
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

% Internal
compile_app_details(App) ->
  compile_app_details1(App, [ 
      {"name", App#app.name}, 
      {"url", App#app.url}, 
      {"routing_param", App#app.routing_param}, 
      {"owners", lists:map(fun(Owner) -> Owner#user.email end, user_apps:get_owners(App))}, 
      {"updated_at", App#app.updated_at},
      {"bee_picker", App#app.bee_picker}
    ], []).

compile_app_details1(_App, [], Acc) -> lists:reverse(Acc);
compile_app_details1(App, [{K,V}|Rest], Acc) ->
  case V of
    % [] -> compile_app_details1(App, Rest, Acc);
    % undefined -> compile_app_details1(App, Rest, Acc);
    _E -> compile_app_details1(App, Rest, [{K,V}|Acc])
  end.