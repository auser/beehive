-module (stress_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

all_test_() ->
  % Add the router ebin
  RootDir = filename:dirname(filename:dirname((filename:dirname(code:which(?MODULE))))),
  code:add_path(filename:join([RootDir, "beehive_router", "ebin"])),
  
  {inparallel,
    [
      {timeout, 60, fun stress_apps/0},
      {timeout, 60, fun start_multiple_bees_for_an_app/0}
    ]
  }.

stress_apps() ->
  Bees = lists:map(fun(N) ->
    Name = lists:flatten(["app", integer_to_list(N)]),
    App = bh_test_util:dummy_app(),
    RealApp = App#app{name = Name},
    apps:save(RealApp),
    app_manager:request_to_start_new_bee_by_app(RealApp, self()),
    receive
      {bee_started_normally, Bee, _App} -> {RealApp, Bee}
    end
  end, lists:seq(1,5)),
  lists:map(fun({_RealApp, Bee}) ->
    {ok, B, Socket} = bee_store:get_bee(Bee#bee.app_name),
    gen_tcp:close(Socket),
    {ok, _, ReturnedData} = bh_test_util:fetch_url(get, [{host, B#bee.host}, {port, B#bee.port}, {path, "/"}]),
    Body = hd(lists:reverse(ReturnedData)),
    ?assertEqual(lists:flatten(["Hello World ", B#bee.app_name]), Body)
  end, lists:reverse(Bees)),
  % Cleanup
  terminate_bees(Bees),
  passed.

start_multiple_bees_for_an_app() ->
  Name = lists:flatten(["multiple_bees_app"]),
  App = bh_test_util:dummy_app(),
  RealApp = App#app{name = Name},
  Num = 3,
  apps:save(RealApp),
  % Start new bees
  Bees = lists:map(fun(_N) ->
    start_new_bee_by_app(RealApp)
  end, lists:seq(1,Num)),
  
  ?assertEqual(Num, length(bees:find_all_by_name(Name))),
  
  terminate_bees(Bees),
  timer:sleep(100),
  passed.

% HELPERS
start_new_bee_by_app(App) ->
  app_manager:request_to_start_new_bee_by_app(App, self()),
  F = fun(This) ->
    receive
      {bee_started_normally, Bee, App} -> 
        {App, Bee};
      {already_started, _} ->
        ok;
      X -> 
        erlang:display({app_manager,request_to_start_new_bee_by_app,X}),
        This(This)
    end
  end,
  F(F).
  
terminate_bees([]) -> ok;
terminate_bees([Bee|Rest]) ->
  case Bee of
    {RealApp, RealBee} -> 
      terminate_bee(RealBee),
      apps:delete(RealApp);
    RealBee when is_record(RealBee, bee) -> terminate_bee(RealBee)
  end,
  terminate_bees(Rest).

terminate_bee(Bee) when is_record(Bee, bee) ->
  {ok, {bee_terminated, _}} = app_manager:terminate_bee(Bee, self()).