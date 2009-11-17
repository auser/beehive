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
-define (CONFIGS_DIR, "test/fixtures/configs").