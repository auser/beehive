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
    not_found -> {error, "App not found"};
    App ->
      AppDetails = compile_app_details(App),
      {"application", AppDetails}
  end;
get(_, _Data) -> 
  All = apps:all(),
  {"apps", lists:map(fun(A) ->
      compile_app_details(A)
    end, All)
  }.

post([], Data) ->
  case auth_utils:get_authorized_user(Data) of
    {error, _,_} = Error -> Error;
    ReqUser ->
      case app_manager:add_application(Data, ReqUser) of
        {ok, App} when is_record(App, app) -> 
          {ok, created};
          % case rebuild_bee(App) of
          %   ok -> {app, misc_utils:to_bin(App#app.name)};
          %   _ -> {error, "there was an error"}
          % end;
        {error, app_exists} -> {error, "App exists already"};
        E -> 
          ?LOG(error, "Unknown error adding app: ~p", [E]),
          {error, "Unknown error adding app. The error has been logged"}
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
    Error -> {app, Error}
  end;
    
post([Name, "expand"], _Data) ->
  case apps:expand_by_name(Name) of
    {ok, _} -> {"app", <<"Expanding...">>};
    _ -> {"app", <<"error">>}
  end;

post(_Path, _Data) -> <<"unhandled">>.

put([Name], Data) ->
  case auth_utils:get_authorized_user(Data) of
    {error, _, _} = Error -> Error;
    _ReqUser ->
      case app_manager:update_application(Name, Data) of
        {ok, App} when is_record(App, app) -> 
          % rebuild_bee(App),
          {updated, App#app.name};
        _ -> {error, "There was an error adding bee"}
      end
  end;
put(_Path, _Data) -> "unhandled".

delete([Name], Data) ->
  case auth_utils:get_authorized_user(Data) of
    {error, _,_} = Error -> Error;
    _ReqUser ->
      case apps:delete(Name) of
        true -> {app, "deleted"};
        _ -> {error, "There was an error deleting app"}
      end
  end;
delete(_Path, _Data) -> "unhandled".

% Internal
compile_app_details(App) ->
  [ 
    {"name", App#app.name}, 
    {"repo_url", App#app.repo_url}, 
    {"routing_param", App#app.routing_param}, 
    {"owners", lists:map(fun(Owner) -> Owner#user.email end, user_apps:get_owners(App))}, 
    {"updated_at", App#app.updated_at},
    {"branch", App#app.branch},
    {"deploy_env", App#app.deploy_env},
    {"dynamic", misc_utils:to_list(App#app.dynamic)},
    {"template", misc_utils:to_list(App#app.template)},
    {"latest_error", case App#app.latest_error of
      undefined -> undefined;
      AppError ->
        [
          {"stage", AppError#app_error.stage},
          {"exit_status", AppError#app_error.exit_status},
          {"stdout", AppError#app_error.stdout},
          {"stderr", AppError#app_error.stderr},
          {"timestamp", AppError#app_error.timestamp}
        ]
    end}
  ].

rebuild_bee(App) ->
  Pid = node_manager:get_next_available(storage),
  Node = node(Pid),
  case rpc:call(Node, beehive_storage_srv, rebuild_bee, [App], 60*1000) of
    {bee_built, _Proplist} -> ok;
    _ -> error
  end.
