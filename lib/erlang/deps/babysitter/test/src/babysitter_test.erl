%%%-------------------------------------------------------------------
%%% File    : babysitter_test.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Jan  7 01:44:19 PST 2010
%%%-------------------------------------------------------------------

-module (babysitter_test).

-include_lib("eunit/include/eunit.hrl").

opts_test_() ->
  [
    ?_assertEqual([], babysitter:build_exec_opts([], [])),
    ?_assertEqual([{cd, "/tmp/hello/world"}], babysitter:build_exec_opts([{cd, "/tmp/hello/world"}], [])),
    ?_assertEqual([], babysitter:build_exec_opts([{doc, "witherspoon"}], [])),
    ?_assertEqual([{env, "HELLO=WORLD"}], babysitter:build_exec_opts([{doc, "witherspoon"}, {env, "HELLO=WORLD"}], []))
  ].

command_test_() ->
  IsoCmd = babysitter:isolate_command(),
  [
    ?_assertEqual(
      lists:flatten(["exec ", IsoCmd, " -C /var/confine -D /bin -D /lib -p 5 -f 5 thin -- -R config.ru start"]),
      babysitter:build_isolate_command([])
    ),
    ?_assertEqual(
      lists:flatten(["exec ", IsoCmd, " -C /var/confine -D /bin -D /lib -p 10 -f 5 thin -- -R config.ru start"]),
      babysitter:build_isolate_command([{num_processes, "10"}])
    ),
    ?_assertEqual(
      lists:flatten(["exec ", IsoCmd, " -C /var/confine -D /bin -D /lib -D /lib/libruby* -p 10 -f 5 thin -- -R config.ru start"]),
      babysitter:build_isolate_command([{num_processes, "10"}, {dirs, ["/lib/libruby*"]}])
    ),
    ?_assertEqual(
      lists:flatten(["exec ", IsoCmd, " -C /var/confine -b /var/skel -p 5 -f 5 thin -- -R config.ru start"]),
      babysitter:build_isolate_command([{skel, "/var/skel"}, {dirs, ["/lib/libruby*"]}])
    ),
    ?_assertEqual(
      lists:flatten(["exec ", IsoCmd, " -C /var/confine -D /bin -D /lib -p 10 -f 10 thin -- -R config.ru --port 1234 start"]),
      babysitter:build_isolate_command([
        {num_processes, "10"}, {files_count, "10"}, {start_command, "thin -- -R config.ru --port [[PORT]] start"},
        {vars, [{"[[PORT]]", "1234"}]}
      ])
    )
  ].