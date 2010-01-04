%%%-------------------------------------------------------------------
%%% File    : app_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:43 PST 2009
%%%-------------------------------------------------------------------

-module (apps_controller).
-include ("beehive.hrl").
-include ("common.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).


get([Name]) ->
  case apps:find_by_name(Name) of
    [] -> {struct, [{"error", ?BINIFY("App not found")}]};
    App ->
      AppDetails = [
        {"name", App#app.name},
        {"url", App#app.url},
        {"routing_param", App#app.routing_param},
        {"owners", lists:map(fun(Owner) -> Owner#user.email end, user_apps:get_owners(App))},
        {"updated_at", App#app.updated_at},
        {"bee_picker", App#app.bee_picker},
        {"min_instances", App#app.min_instances},
        {"max_instances", App#app.max_instances},
        {"timeout", App#app.timeout},
        {"sticky", App#app.sticky},
        {"latest_sha", App#app.sha}
      ],
      {struct, [{
        Name, 
        {struct, ?BINIFY(AppDetails)}
      }]}
  end;
get(_) -> 
  All = apps:all(),
  {struct, [{
    "apps",
    lists:map(fun(A) ->
      Details = compile_app_details(A),
      {struct, ?BINIFY(Details)}
    end, All)
  }]}.

post([], Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> 
      ?JSON_ERROR("No user defined or invalid token");
      % {struct, [{"error", misc_utils:to_bin("No user defined or invalid token")}]};
    ReqUser ->
      case apps:create(Data) of
        {ok, App} when is_record(App, app) -> 
          user_apps:create(ReqUser, App),
          {struct, ?BINIFY([{"app", misc_utils:to_bin(App#app.name)}])};
        {error, app_exists} -> ?JSON_ERROR("App exists already");
        E -> 
          ?LOG(error, "Unknown error adding app: ~p", [E]),
          ?JSON_ERROR("Unknown error adding app. The error has been logged")
      end
  end;

  % Not sure about this... yet as far as authentication goes
post([Name, "restart"], _Data) ->
  Response = case ?NOTIFY({app, restart, Name}) of
    ok -> {"app", <<"restarting">>};
    _E -> {"app", <<"error">>}
  end,
  {struct, ?BINIFY([Response])};

% Not sure about this... yet as far as authentication goes
post([Name, "deploy"], _Data) ->
  Response = case apps:update_by_name(Name) of
    {ok, _} -> {"app", <<"updated">>};
    _ -> {"app", <<"error">>}
  end,
  {struct, ?BINIFY([Response])};
    
post(_Path, _Data) -> "unhandled".

put([Name], Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> 
      {struct, [{"error", misc_utils:to_bin("No user defined or invalid token")}]};
    _ReqUser ->
      case apps:update(Name, Data) of
        {updated, App} when is_record(App, app) -> ?JSON_RETURN("updated", App#app.name);
        _ -> ?JSON_ERROR("There was an error adding bee")
      end
  end;
put(_Path, _Data) -> "unhandled".

delete([Name], _Data) ->
  case apps:delete(Name) of
    ok -> 
      Message = misc_utils:to_bin(lists:append(["Deleted app ", Name])),
      {struct, [{"message", Message}]};
    _ -> 
      {struct, [{"error", misc_utils:to_bin("There was an error deleting the application")}]}
  end;

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