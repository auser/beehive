-module(web_utils).
-include ("beehive.hrl").
-include ("http.hrl").
-compile (export_all).

% Private
convert_to_struct(RawData) ->
  lists:map(fun({BinKey, BinVal}) ->
      Key = misc_utils:to_atom(BinKey),
      Val = misc_utils:to_list(BinVal),
      {Key, Val}
    end, RawData).

% jsonify the struct
jsonify(JsonifiableBody) ->
  [ ?JSON_ENCODE({
        struct, [
          JsonifiableBody
        ]
    })
  ].

to_json(V) -> json_encode(prepare_for_json(V)).

prepare_for_json(Int) when is_integer(Int) -> Int;
prepare_for_json(Float) when is_float(Float) -> Float;
prepare_for_json(Atom) when is_atom(Atom) -> Atom;
prepare_for_json(Array) when is_list(Array) ->
  case io_lib:printable_list(Array) of
    true ->
      erlang:list_to_binary(Array); % This is a string, not a list
    false ->
      list_to_json(Array, []) % This is a list, of some sort
  end;
prepare_for_json(Tuple) when is_tuple(Tuple) ->
  tuple_to_json(Tuple, erlang:size(Tuple), []);
prepare_for_json(V) -> V.

list_to_json([], Acc) -> lists:reverse(Acc);
list_to_json([{_Key, _Value}|_Rest] = List, Acc) -> {struct, proplist_to_json(List, Acc)};
list_to_json([H|Rest], Acc) -> list_to_json(Rest, [prepare_for_json(H)|Acc]).

proplist_to_json([], Acc) -> lists:reverse(Acc);
proplist_to_json([{Key, Value}|Rest], Acc) ->
  ValidKey    = prepare_for_json(Key),
  ValidValue  = prepare_for_json(Value),
  proplist_to_json(Rest, [{ValidKey, ValidValue}|Acc]).

tuple_to_json(_Tuple, 0, Acc) ->  {struct, [erlang:list_to_tuple(Acc)]};
tuple_to_json(Tuple, CurrPos, Acc) ->
  Ele = prepare_for_json(element(CurrPos, Tuple)),
  tuple_to_json(Tuple, CurrPos - 1, [Ele|Acc]).

json_encode(Value) -> mochijson2:encode(Value).

ip_to_list({IP, _Port}) -> ip_to_list(IP);
ip_to_list({A, B, C, D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A, B, C, D]));
ip_to_list(List) -> List.
