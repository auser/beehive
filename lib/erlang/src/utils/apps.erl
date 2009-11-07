-module (apps).
-include ("router.hrl").
-include ("common.hrl").
-export ([
  search_for_application_value/3,
  lookup/2,
  store/3,
  delete/2,
  all/1
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
  
% Lookups
lookup(app, Name) ->
  case ?KVSTORE:lookup(?APP_DB, Name) of
    error -> [];
    Else -> Else
  end;
lookup(backend2pid, Name) ->
  case ?KVSTORE:lookup(?BACKEND2PID_DB, Name) of
    error -> 
      % This backend does not have a backend2pid_list yet, so let's create one
      ?KVSTORE:store(?BACKEND2PID_DB, Name, []),
      [];
    E ->  E
  end;
lookup(pid, Pid) ->
  case ?KVSTORE:lookup(?PID2BACKEND_DB, Pid) of
    error -> no_exist;
    B -> B
  end;
lookup(backends, Name) -> 
 case ?KVSTORE:lookup(?BACKEND_DB, Name) of
   error -> [];
   Else -> Else
 end;
lookup(_, _Name) -> undefined.

% Store
store(app, Name, App) -> ?KVSTORE:store(?APP_DB, Name, App);
store(backend2pid, Backend, NewPidlist) -> ?KVSTORE:store(?BACKEND2PID_DB, Backend#backend.app_name, NewPidlist);
store(pid, Pid, Backend) -> ?KVSTORE:store(?PID2BACKEND_DB, Pid, Backend);
store(backend, Name, Backends) -> ?KVSTORE:store(?BACKEND_DB, Name, Backends);
store(_, _, _) -> ok.

delete(pid, Pid) -> ?KVSTORE:delete(?PID2BACKEND_DB, Pid);
delete(_, _) -> ok.

% All
all(apps)     -> lists:flatten(lists:map(fun({_K,V}) -> V end, ?KVSTORE:all(?APP_DB)));
all(backends) -> lists:flatten(lists:map(fun({_K,V}) -> V end, ?KVSTORE:all(?BACKEND_DB)));
all(_Else)  -> [].
