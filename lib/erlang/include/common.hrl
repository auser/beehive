-define(LOG(LogLevel, LogFormat, LogArgs), 
        % try
        case event_manager:notify({log, LogLevel, LogFormat, LogArgs, ?FILE, ?LINE}) of
          ok -> ok;
          {error, _} -> throw({error, logging_exception})
        end
        ).

-ifdef (debug).
-define (BENCHMARK_LOG (Msg, Mod, Fun, Args), fun() ->
  {Time, Value} = timer:tc(Mod, Fun, Args),
  ?LOG(benchmark, "~p microseconds ~p ~p:~p/~p", [Time, Msg, Mod, Fun, erlang:length(Args)]),
  Value
end()).
-else.
-define (BENCHMARK_LOG (_Msg, Mod, Fun, Args),
  erlang:apply(Mod, Fun, Args)).
-endif.

-define (CONFIG_FILE, case os:getenv("ROUTER_CONFIG") of
	undefined -> "include/config.cfg";
	F -> F
end).

-define (BEEHIVE_HOME, os:getenv("HOME")).
-define (FIXTURES_DIR, bh_file_utils:relative_path("test/fixtures")).
% Figure this out... Hm
-define (SHELL_SCRIPTS_DIR, filename:join([?BEEHIVE_HOME, "./shell_templates/"])).

-define (SHELL_SCRIPT_PATH (Name), filename:join([?SHELL_SCRIPTS_DIR, lists:append([Name, ".sh"])])).
-define (SHELL_SCRIPT (Name), fun() ->
  case file:read_file(?SHELL_SCRIPT_PATH(Name)) of
    {ok, Binary} -> erlang:binary_to_list(Binary);
    {error, Reason} -> 
      ?LOG(error, "Could not file shell_script named: ~p", [Name]),
      throw({error, shell_script, Reason})
    end
 end()
).
-define (TEMPLATE_SHELL_SCRIPT (Name, Params), fun() ->
    string_utils:template_command_string(?SHELL_SCRIPT(Name), Params)
  end()
).
-define (TEMPLATE_SHELL_SCRIPT_PARSED (Name, Params), misc_utils:shell_fox(Name, Params)).