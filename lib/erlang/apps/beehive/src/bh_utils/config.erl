-module (config).
-include ("common.hrl").
-compile (export_all).


% Find the application config value
search_for_application_value(Param, Default, App) ->
  case search_for_application_value_from_environment(App, Param) of
    false ->
      case search_for_application_value_on_env(App, Param) of
        false ->
          case search_for_application_value_from_config(App, Param) of
            false -> Default;
            C -> C
          end;
        B -> B
      end;
    A -> A
  end.
  
search_for_application_value_on_env(App, Param) ->
  case application:get_env(App, Param) of
    undefined         -> false;
    {ok, undefined}   -> false;
    {ok, V}    -> V
  end.

search_for_application_value_from_config(_App, Param) ->
	case config:get(Param) of
		{error, _} -> false;
		{ok, V} -> V
	end.

search_for_application_value_from_environment(_App, Param) ->
  EnvParam = string:to_upper(lists:flatten(["beehive_", erlang:atom_to_list(Param)])),
  case os:getenv(EnvParam) of
    false -> false;
    E -> E
  end.
  
%%--------------------------------------------------------------------
%% Function: Read the config file () -> {ok, Config} | 
%%                                      {error, Reason}
%% Description: Read the configuration data
%%--------------------------------------------------------------------
read() ->
  ConfigFile = case application:get_env(beehive, config_file) of
    undefined -> ?USER_OR_BH("include/config.cfg");
    {ok, Cf} -> Cf
  end,
  read(ConfigFile).

read(ConfigFile) ->
  case (catch yaml:parse_file(ConfigFile)) of
    {'EXIT', _} -> [];
    {error, _} -> [];
    C -> misc_utils:atomize(C, [])
  end.
  
%%--------------------------------------------------------------------
%% Function: get (Key, Config) -> {error, not_found} |
%%                                {ok, Value}
%% Description: Get the value of a config element
%%--------------------------------------------------------------------
get(Key) -> get(Key, read()).
get(_Key, []) -> {error, not_found};
get(Key, [{Key, Value} | _Config]) -> {ok, Value};
get(Key, [{_Other, _Value} | Config]) -> get(Key, Config).

%%--------------------------------------------------------------------
%% Function: get_or_default (Key, Default, Config) -> Value
%% Description: Get the config element or default
%%--------------------------------------------------------------------
get_or_default(Key, Default, Config) ->
  case get(Key, Config) of
    {ok, undefined} -> Default;
    {ok, V} -> V;
    {error, not_found} -> Default
  end.