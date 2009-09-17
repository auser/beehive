-module (app_discovery).
-include ("beehive.hrl").
-export ([discover_local_apps/0]).

discover_local_apps() ->
  ?TRACE("Directory: ~p~n", [?ROOT_DIR_PREFIX]),
  [].