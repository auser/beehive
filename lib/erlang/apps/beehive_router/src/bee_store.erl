%%%-------------------------------------------------------------------
%%% File    : bee_store.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Wed Oct  7 22:37:21 PDT 2009
%%%-------------------------------------------------------------------

-module (bee_store).

-include ("beehive.hrl").
-include ("router.hrl").
-include ("common.hrl").
-include_lib("kernel/include/inet.hrl").

-export([get_bee/1]).

%%-------------------------------------------------------------------
%% @spec (Hostname) ->    {ok, Bee, Socket}
%%                        | {error, Reason}
%% @doc Get the bee and a socket connection to the bee
%%
%% @end
%%-------------------------------------------------------------------
get_bee(default) -> get_default_app_or_rest();
get_bee([Hostname|_Rest] = List) ->
 % For now, since we are only looking up on a single char_list
 % we have to hack it
 % LAME!
  case io_lib:char_list(List) of
    true -> get_bee(List, date_util:now_to_seconds());
    false -> get_bee(Hostname, date_util:now_to_seconds())
  end.

%% Choose an available back-end host. If a bee cannot be found within
%% the requested time (before the CONNECTION_TIMEOUT) then we pass
%% back a timeout
get_bee(Hostname, TimeofRequest) ->
  TOTime = date_util:now_to_seconds() - TimeofRequest,
  case TOTime - TimeofRequest > ?CONNECTION_TIMEOUT of
    true -> {error, timeout};
    false ->
      case get_bee_by_hostname(Hostname) of
        {ok, _Bee, _Socket} = Tuple ->
          ?LOG(debug, "get_bee_by_hostname returned: ~p", [Tuple]),
          Tuple;
        ?MUST_WAIT_MSG ->
          timer:sleep(500),
          get_bee(Hostname);
        {error, Reason} ->
          ?LOG(debug, "get_bee_by_hostname error: ~p", [Reason]),
          {error, Reason}
      end
  end.

%%-------------------------------------------------------------------
%% @spec (Hostname) ->      MUST_WAIT_MSG
%%                        | {error, Reason}
%% @doc In the case that there are no 'ready' bees, fire this event
%%
%% @end
%%-------------------------------------------------------------------
handle_no_ready_bees(Hostname) ->
  case apps:find_by_name(Hostname) of
    not_found -> {error, unknown_app};
    App ->
      case App#app.latest_error of
        undefined ->
          ?NOTIFY({app, request_to_start_new_bee, App}),
          ?MUST_WAIT_MSG;
        _E ->
          {error, cannot_choose_bee}
      end
  end.

get_default_app_or_rest() ->
  DefaultAppName = config:search_for_application_value(base_app, router),
  case DefaultAppName of
    router ->
      Port = config:search_for_application_value(default_app_port, 4999),
      Host = {127,0,0,1},
      Id = {default, Host, Port},

      Bee = #bee{
        id = Id, port = Port, host = Host, app_name = default
      },
      try_to_connect(Bee, fun(Error) -> Error end);
    Else ->
      case get_bee_by_hostname(Else) of
        {ok, _Bee, _Socket} = Tuple -> Tuple;
        _Else -> {error, could_not_start}
      end
  end.

% Fetch a running bee by the hostname
get_bee_by_hostname(Hostname) ->
  case get_bees_by_hostname(Hostname) of
    {error, Reason} -> {error, Reason};
    [] -> handle_no_ready_bees(Hostname);
    ?MUST_WAIT_MSG -> ?MUST_WAIT_MSG;
    {ok, _App, Bees} ->
      PreferredStrategy =
        config:search_for_application_value(bee_strategy, random),
      SortedBees = bee_strategies:PreferredStrategy(Bees),
      case choose_from_ready_bees(SortedBees) of
        ?MUST_WAIT_MSG -> ?MUST_WAIT_MSG;
        {ok, _Bee, _Socket} = Tuple -> Tuple;
        Else -> {error, Else}
      end
  end.

% Get all the bees marked as 'ready' for the Hostname
get_bees_by_hostname(Hostname) ->
  case apps:find_by_name(Hostname) of
    not_found -> {error, app_not_found};
    App when is_record(App, app) ->
      case (catch bees:find_all_by_name(Hostname)) of
        [] -> handle_no_ready_bees(Hostname);
        {'EXIT', {_, {no_exists, Err}}} ->
          ?LOG(error, "No exists for bees: ~p", [Err]),
          ?NOTIFY({db, database_not_initialized, bees}),
          ?MUST_WAIT_MSG;
        {'EXIT', _} ->
          ?LOG(error, "Undefined error with choose_bee", []),
          ?MUST_WAIT_MSG;
        Bees ->
          ReadyBees = lists:filter(fun(B) -> (catch B#bee.status =:= ready) end, Bees),
          ?LOG(debug, "ReadyBees: ~p", [ReadyBees]),
          case ReadyBees of
            [] -> handle_no_ready_bees(Hostname);
            _ -> {ok, App, ReadyBees}
          end
      end;
    _ -> {error, unknown}
  end.

choose_from_ready_bees([]) -> ?MUST_WAIT_MSG;
choose_from_ready_bees([Bee|Rest]) ->
  try_to_connect(Bee, fun(_) ->
                            ?NOTIFY({bee, cannot_connect, Bee#bee.id}),
                            choose_from_ready_bees(Rest)
                      end).

try_to_connect(#bee{host = Host, port = Port} = Bee, Failure) ->
  SockOpts = [binary, {active, false}, {packet, raw} ],
  case gen_tcp:connect(Host, Port, SockOpts, ?NODE_CONNECT_TIMEOUT) of
    {ok, Socket} -> {ok, Bee, Socket};
    Else -> Failure(Else)
  end.
