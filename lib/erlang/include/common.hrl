-define(LOG(Log_Level, Log_Format, Log_Args), 
        begin 
            {{Log_Y,Log_M,Log_D},{Log_Hr,Log_Min,Log_Sec}} = erlang:localtime(),
            io:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w ~w ~w\t" ++ Log_Format ++ "\n",
                      [Log_Y, Log_M, Log_D, Log_Hr,Log_Min,Log_Sec,?MODULE,Log_Level|Log_Args]),
            ok
        end ).


-define (CONFIG_FILE, case os:getenv("ROUTER_CONFIG") of
	undefined -> "include/config.cfg";
	F -> F
end).

-define (FIXTURES_DIR, 
    filename:join([filename:absname(""), "test/fixtures"])
  ).

-define (CONFIGS_DIR, "test/fixtures/configs").