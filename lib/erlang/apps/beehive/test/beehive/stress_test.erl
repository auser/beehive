-module (stress_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

all_test_() ->
  {timeout, 3000, [fun stress_apps/0]}.

stress_apps() ->
  Bees = lists:map(fun(N) ->
    Name = lists:flatten(["app", integer_to_list(N)]),
    App = bh_test_util:dummy_app(),
    app_manager:request_to_start_new_bee_by_app(App#app{name = Name}, self()),
    receive
      {bee_started_normally, Bee} -> Bee
    end
  end, lists:seq(1,5)),
  lists:map(fun(Bee) ->
    {ok, _, ReturnedData} = bh_test_util:fetch_url(get, [{host, Bee#bee.host}, {port, Bee#bee.port}, {path, "/"}]),
    Body = hd(lists:reverse(ReturnedData)),
    ?assertEqual(lists:flatten(["Hello World ", Bee#bee.app_name]), Body)
  end, Bees),
  lists:map(fun(Bee) ->
    app_manager:request_to_terminate_bee(Bee, self()),
    timer:sleep(100),
    receive
      {bee_terminated, _} -> ok;
      X -> erlang:display({terminate,got,else,X})
    end
  end, Bees),
  passed.