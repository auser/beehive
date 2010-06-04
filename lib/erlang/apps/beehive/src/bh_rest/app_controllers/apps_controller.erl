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
-export ([get/2, post/2, put/2, delete/2]).


get([Name], _Data) ->
  case apps:find_by_name(Name) of
    [] -> {"error", ?BINIFY("App not found")};
    App ->
      AppDetails = [
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
      {Name, AppDetails}
  end;
get(_, _Data) -> 
  All = apps:all(),
  {"apps", lists:map(fun(A) ->
      Details = compile_app_details(A),
      {A#app.name, Details}
    end, All)
  }.

post([], Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> 
      ?JSON_ERROR("No user defined or invalid token");
      % {struct, [{"error", misc_utils:to_bin("No user defined or invalid token")}]};
    ReqUser ->
      case apps:create(Data) of
        {ok, App} when is_record(App, app) -> 
          user_apps:create(ReqUser, App),
          {app, misc_utils:to_bin(App#app.name)};
        {error, app_exists} -> ?JSON_ERROR("App exists already");
        E -> 
          ?LOG(error, "Unknown error adding app: ~p", [E]),
          ?JSON_ERROR("Unknown error adding app. The error has been logged")
      end
  end;

  % Not sure about this... yet as far as authentication goes
post([Name, "restart"], _Data) ->
  case apps:restart_by_name(Name) of
    {ok, _} -> {"app", <<"restarting">>};
    _E -> {"app", <<"error">>}
  end;

% Not sure about this... yet as far as authentication goes
post([Name, "deploy"], _Data) ->
  case apps:update_by_name(Name) of
    {ok, _} -> {app, <<"updated">>};
    _ -> {app, <<"error">>}
  end;
    
post([Name, "expand"], _Data) ->
  case apps:expand_by_name(Name) of
    {ok, _} -> {"app", <<"Expanding...">>};
    _ -> {"app", <<"error">>}
  end;

post(_Path, _Data) -> <<"unhandled">>.

put([Name], Data) ->
  case auth_utils:get_authorized_user(Data) of
    false -> 
      {"error", misc_utils:to_bin("No user defined or invalid token")};
    _ReqUser ->
      case apps:update(Name, Data) of
        {updated, App} when is_record(App, app) -> ?JSON_MSG("updated", App#app.name);
        _ -> ?JSON_ERROR("There was an error adding bee")
      end
  end;
put(_Path, _Data) -> "unhandled".

delete([Name], Data) ->
  io:format("Data: ~p~n", [Data]),
  case auth_utils:get_authorized_user(Data) of
    false -> ?JSON_ERROR("No user defined or invalid token");
    _ReqUser ->
      case apps:delete(Name) of
        ok -> ?JSON_MSG("app", "deleted");
        _ -> ?JSON_ERROR("There was an error deleting app")
      end
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
      {"type", misc_utils:to_list(App#app.type)},
      {"template", misc_utils:to_list(App#app.template)},
      {"bee_picker", App#app.bee_picker}
    ], []).

compile_app_details1(_App, [], Acc) -> lists:reverse(Acc);
compile_app_details1(App, [{K,V}|Rest], Acc) ->
  case V of
    % [] -> compile_app_details1(App, Rest, Acc);
    % undefined -> compile_app_details1(App, Rest, Acc);
    _E -> compile_app_details1(App, Rest, [{K,V}|Acc])
  end.