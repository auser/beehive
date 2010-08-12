-module (stress_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

all_test_() ->
  % Add the router ebin
  RootDir = filename:dirname(filename:dirname((filename:dirname(code:which(?MODULE))))),
  code:add_path(filename:join([RootDir, "beehive_router", "ebin"])),
  {timeout, 3000, [fun stress_apps/0]}.

stress_apps() ->
  Bees = lists:map(fun(N) ->
    Name = lists:flatten(["app", integer_to_list(N)]),
    App = bh_test_util:dummy_app(),
    RealApp = App#app{name = Name},
    apps:save(RealApp),
    app_manager:request_to_start_new_bee_by_app(RealApp, self()),
    receive
      {bee_started_normally, Bee} -> {RealApp, Bee}
    end
  end, lists:seq(1,5)),
  lists:map(fun({_RealApp, Bee}) ->
    {ok, B, Socket} = bee_store:get_bee(Bee#bee.app_name),
    gen_tcp:close(Socket),
    {ok, _, ReturnedData} = bh_test_util:fetch_url(get, [{host, B#bee.host}, {port, B#bee.port}, {path, "/"}]),
    Body = hd(lists:reverse(ReturnedData)),
    ?assertEqual(lists:flatten(["Hello World ", B#bee.app_name]), Body)
  end, Bees),
  % Cleanup
  lists:map(fun({RealApp, Bee}) ->
    app_manager:request_to_terminate_bee(Bee, self()),
    timer:sleep(100),
    apps:delete(RealApp),
    receive
      {bee_terminated, _} -> ok;
      X -> erlang:display({terminate,got,else,X})
    end
  end, Bees),
  passed.