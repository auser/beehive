-define(LOG(Log_Level, Log_Format, Log_Args), 
        % try
        case event_manager:notify({log, Log_Level, Log_Format, Log_Args}) of
          ok -> ok;
          {error, _} -> throw({error, logging_exception})
        end
        ).

-define (CONFIG_FILE, case os:getenv("ROUTER_CONFIG") of
	undefined -> "include/config.cfg";
	F -> F
end).

-define (FIXTURES_DIR, file_utils:relative_path("test/fixtures")).
% Figure this out... Hm
-define (CONFIGS_DIR, file_utils:relative_path("webapp/config")).
-define (SHELL_SCRIPTS_DIR, file_utils:relative_path("./../shell")).

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