-module (config).
-include ("common.hrl").
-compile (export_all).

-define (ETS_CONFIG_TABLE, 'in_memory_config_file').

% TODO: IMPLEMENT THE IN-MEMORY CONFIG
init() ->
  case catch ets:info(?ETS_CONFIG_TABLE) of
    undefined -> ets:new(?ETS_CONFIG_TABLE, [set, named_table, public]);
    _ -> ok
  end.

% Find the application config value
search_for_application_value(Param) -> search_for_application_value(Param, undefined).
search_for_application_value(Param, Default) ->
  case search_for_application_value_from_environment(Param) of
    false ->
      case search_for_application_value_from_config(Param) of
        false ->
          case search_for_application_value_on_env(Param) of
            false -> Default;
            C -> C
          end;
        B -> B
      end;
    A -> A
  end.
  
search_for_application_value_on_env(Param) ->
  case application:get_env(beehive, Param) of
    undefined         -> false;
    {ok, undefined}   -> false;
    {ok, V}    -> V
  end.

search_for_application_value_from_config(Param) ->
	case config:get(Param) of
	  undefined -> false;
		{error, _} -> false;
		{ok, undefined} -> false;
		{ok, V} -> V
	end.

% BEEHIVE_HOME === config:search_for_application_value(home, "/tmp/beehive"),
search_for_application_value_from_environment(Param) ->
  EnvParam = string:to_upper(lists:flatten(["beehive_", erlang:atom_to_list(Param)])),
  case os:getenv(EnvParam) of
    false -> false;
    E -> 
      case E of
        "true" -> true;
        "false" -> false;
        Otherwise -> Otherwise
      end
  end.
  
%%--------------------------------------------------------------------
%% Function: Read the config file () -> {ok, Config} | 
%%                                      {error, Reason}
%% Description: Read the configuration data
%%--------------------------------------------------------------------
read() ->
  ConfigFile = find_config_file(),
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

%%--------------------------------------------------------------------
%% Function: find_config_file () -> Path
%% Description: Get the config file by looking in default/known locations
%%--------------------------------------------------------------------
find_config_file() ->
  case find_config_file_in_app() of
    false ->
      case find_config_file_in_beehive_root() of
        false ->
          case find_config_file_in_etc() of
            false -> undefined;
            E3 -> E3
          end;
        E2 -> E2
      end;
    E1 -> E1
  end.

find_config_file_in_app() ->
  case application:get_env(beehive, config_file) of
    {ok, Cf} -> 
      case is_file(Cf) of
        false -> false;
        Else -> Else
      end;
    undefined -> false
  end.
  
find_config_file_in_beehive_root() ->
  case is_file(?BEEHIVE_DIR("etc/beehive.conf")) of
    false -> false;
    E -> E
  end.
  
find_config_file_in_etc() ->
  case is_file("/etc/beehive.conf") of
    false -> false;
    E -> E
  end.

is_file(Path) ->
  case filelib:is_file(Path) of
    true -> Path;
    _ -> false
  end.