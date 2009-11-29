-module (apps).
-include ("beehive.hrl").
-include ("common.hrl").
-export ([
  search_for_application_value/3
]).

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
		V -> V
	end.

search_for_application_value_from_environment(Param, Default) ->
  EnvParam = string:to_upper(erlang:atom_to_list(Param)),
  case os:getenv(EnvParam) of
    false -> Default;
    E -> E
  end.  