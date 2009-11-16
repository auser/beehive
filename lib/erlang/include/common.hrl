-define(LOG(Log_Level, Log_Format, Log_Args), 
        % try
        case event_manager:notify({log, Log_Level, Log_Format, Log_Args}) of
          ok -> ok;
          {error, _} -> throw({error, logging_exception})
        end
        ).
        % catch
        %   _:Reason ->
        %     io:format("Error with LOG: ~p~n", [Reason]),
        %     {{Log_Y,Log_M,Log_D},{Log_Hr,Log_Min,Log_Sec}} = erlang:localtime(),
        %     io:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w ~w ~w\t" ++ Log_Format ++ "\n", [Log_Y, Log_M, Log_D, Log_Hr,Log_Min,Log_Sec,?MODULE,Log_Level|Log_Args])
        % end).


-define (CONFIG_FILE, case os:getenv("ROUTER_CONFIG") of
	undefined -> "include/config.cfg";
	F -> F
end).

-define (FIXTURES_DIR, file_utils:relative_path("test/fixtures")).
-define (CONFIGS_DIR, "test/fixtures/configs").