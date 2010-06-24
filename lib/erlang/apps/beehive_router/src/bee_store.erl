%%%-------------------------------------------------------------------
%%% File    : bee_store.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct  7 22:37:21 PDT 2009
%%%-------------------------------------------------------------------

-module (bee_store).

-include ("beehive.hrl").
-include ("router.hrl").
-include ("common.hrl").
-include_lib("kernel/include/inet.hrl").

-export([get_bee/1]).

get_bee(Hostname) -> get_bee(Hostname, date_util:now_to_seconds()).

%% Choose an available back-end host
get_bee([Hostname|_Rest], TimeofRequest) -> get_bee(Hostname, TimeofRequest);
get_bee(Hostname, TimeofRequest) -> 
  TOTime = date_util:now_to_seconds() - TimeofRequest,
  case TOTime - TimeofRequest > ?CONNECT_TIMEOUT of
    true -> {error, timeout};
    false ->
      case get_bee_by_hostname(Hostname) of
        {ok, _Bee} = Tuple -> Tuple;
        ?MUST_WAIT_MSG ->
          timer:sleep(500),
          get_bee(Hostname);
        {error, Reason} -> {error, Reason}
      end
  end.
  
handle_no_ready_bees(Hostname) ->
  case apps:find_by_name(Hostname) of
    not_found ->
      {error, unknown_app};
    App ->
      case App#app.latest_error of
        undefined ->
          erlang:display({app, choose_bee, App}),
          ?NOTIFY({app, request_to_start_new_bee, App}),
          ?MUST_WAIT_MSG;
        _E ->
          {error, cannot_choose_bee}
      end
  end.

% Choose from the list of bees
% Here is the logic to choose a bee from the list of bees
% The user defines the preferred strategy for choosing backends when starting
% the router with the -g option
% i.e. start_beehive.com -g random
% If the application defines it's own routing mechanism with the routing_param
% then that is used to choose the backend, otherwise the default router param will
% be used
choose_from_ready_bees([], _AppMod, _RoutingParameter) -> ?MUST_WAIT_MSG;
choose_from_ready_bees(Bees, Mod, _AppRoutingParam) ->
  PreferredStrategy = config:search_for_application_value(bee_strategy, random),
  Fun = PreferredStrategy,
  % TODO: Reimplement
  % Fun = case AppRoutingParam of
  %   undefined -> PreferredStrategy;
  %   F -> F
  % end,
  Bee = Mod:Fun(Bees),
  {ok, Bee}.

get_default_app_or_rest(From, State) ->
  DefaultAppName = config:search_for_application_value(base_app, router),
  case DefaultAppName of
    router ->
      Port = config:search_for_application_value(default_app_port, 4999), 
      Host = {127,0,0,1},
      Id = {default, Host, Port},

      #bee{ 
        id = Id, port = Port, host = Host, app_name = default
      };
    Else ->
      case get_bee_by_hostname(Else, From, State) of
        {ok, TheBee} -> {ok, TheBee};
        _Else -> {error, could_not_start}
      end
  end.

% Fetch a running bee by the hostname
get_bee_by_hostname(Hostname, From, State) ->
  case get_bees_by_hostname(Hostname) of
    {error, Reason} -> {error, Reason};
    [] -> handle_no_ready_bees(Hostname);
    {ok, App, Bees} ->
      case choose_bee(Bees) of
        ?MUST_WAIT_MSG -> ?MUST_WAIT_MSG;
    	  {ok, Bee} -> Bee;
    	  Else -> {error, Else}
    	end
  end.

% Get all the bees marked as 'ready' for the Hostname
get_bees_by_hostname(Hostname) ->
  case apps:find_by_name(Hostname) of
    not_found -> {error, not_found};
    App when is_record(App, app) ->
      case (catch bees:find_all_by_name(Hostname)) of
        [] -> handle_no_ready_bees(Hostname);
        {'EXIT', {_, {no_exists, Err}}} ->
          ?LOG(error, "No exists for bees: ~p", [Err]),
          ?NOTIFY({db, database_not_initialized, bees}),
          ?MUST_WAIT_MSG;
        {'EXIT', _} ->
          ?LOG(error, "Undefined error with choose_bee", []),
          ?MUST_WAIT_MSG;
        Bees ->
          {ok, App, lists:filter(fun(B) -> (catch B#bee.status =:= ready) end, Bees)}
      end;
    _ -> {error, unknown}
  end.

% These are commands that can be specified on an application
% bee_picker is the custom module to choose the bee from
% routing_param is the name of the method in the bee_picker
% Defaults to bee_strategies:random if none are specified on the app
pick_mod_and_meta_from_app(App) when is_record(App, app) ->
  Mod = case App#app.bee_picker of
    undefined -> config:search_for_application_value(bee_picker, bee_strategies);
    E -> E
  end,
  R = case App#app.routing_param of
    undefined -> config:search_for_application_value(bee_strategy, random);
    El -> El
  end,
  {Mod, R}.