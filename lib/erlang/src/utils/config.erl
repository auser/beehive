-module (config).
-include ("common.hrl").
-compile (export_all).


% Find the application config value
search_for_application_value(Param, Default, App) ->
  case application:get_env(App, Param) of
    undefined         -> search_for_application_value_from_config(Param, Default);
    {ok, undefined}   -> search_for_application_value_from_config(Param, Default);
    {ok, V}    -> V
  end.

search_for_application_value_from_config(Param, Default) ->
	case config:get(Param) of
		{error, _} -> search_for_application_value_from_environment(Param, Default);
		{ok, V} -> V
	end.

search_for_application_value_from_environment(Param, Default) ->
  EnvParam = string:to_upper(erlang:atom_to_list(Param)),
  case os:getenv(EnvParam) of
    false -> Default;
    E -> E
  end.
  
%%--------------------------------------------------------------------
%% Function: Read the config file () -> {ok, Config} | 
%%                                      {error, Reason}
%% Description: Read the configuration data
%%--------------------------------------------------------------------
read() ->
  ConfigFile = case application:get_env(beehive, config_file) of
    undefined -> ?CONFIG_FILE;
    {ok, Cf} -> Cf
  end,
  case file:consult(ConfigFile) of
    {ok, C} -> C;
    {error, _} -> [];
    Err -> Err
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