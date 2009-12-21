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

-define (FIXTURES_DIR, file_utils:relative_path("test/fixtures")).
% Figure this out... Hm
-define (CONFIGS_DIR, file_utils:relative_path("webapp/config")).
-define (SHELL_SCRIPTS_DIR, file_utils:relative_path("./shell_templates/")).

-define (SHELL_SCRIPT_PATH (Name), filename:join([?SHELL_SCRIPTS_DIR, lists:append([Name, ".sh"])])).
-define (SHELL_SCRIPT (Name), fun() ->
  {ok, Binary} = file:read_file(?SHELL_SCRIPT_PATH(Name)),
  erlang:binary_to_list(Binary)
 end()
).
-define (TEMPLATE_SHELL_SCRIPT (Name, Params), fun() ->
    string_utils:template_command_string(?SHELL_SCRIPT(Name), Params)
  end()
).
-define (TEMPLATE_SHELL_SCRIPT_PARSED (Name, Params), misc_utils:shell_fox(Name, Params)).