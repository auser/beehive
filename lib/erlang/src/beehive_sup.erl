-module (beehive_sup).
-include ("beehive.hrl").
-include ("beehive_app.hrl").
-behaviour(supervisor).

-export([ start/2,
          init/1,
          stop/1]).
          
-export ([compile_applications/1]).

start(Type, Args) ->  
  supervisor:start_link(?MODULE, [Type, Args]).

init([_Type, Args]) ->
  io:format("starting ~p with ~p~n", [?MODULE, Args]),
  
  BeehiveLoggerSup  = { beehive_logger,  {beehive_logger, start_link, [Args]}, permanent,2000,worker,[]},
  RegistryApp       = { app_registry,  {app_registry_srv, start_link, [Args]}, permanent,2000,worker,[]},
  
  FullApplicationList = compile_applications( [
                                                {no_log, BeehiveLoggerSup},
                                                {no_registry, RegistryApp}
                                              ]),
  
  {ok,{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME}, FullApplicationList}}.
  
stop(Args) ->
  lists:map(fun(Term) -> Term:stop(Args) end, [
                                            beehive_logger,
                                            app_registry_srv
                                          ]).
                                          
%%====================================================================
%% PRIVATE
%%====================================================================

compile_applications(Applications) -> compile_applications(Applications, []).
compile_applications([], Acc) -> lists:reverse(Acc);
compile_applications([H|T], Acc) ->
  {Switch, AppDef} = H,
  compile_applications(T, merge_unless_true(Switch, [AppDef], Acc)).
  
merge_unless_true(Param, Merge, Into) ->
  case application:get_env(beehive, Param) of
    undefined -> lists:append([Merge, Into]);
    {ok, CC} -> case CC of
      true -> Into;
      _ -> lists:append([Into, Merge])
    end
  end.