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
list_to_json([H|Rest], Acc) -> list_to_json(Rest, [prepare_for_json(H)|Acc]).

tuple_to_json(_Tuple, 0, Acc) ->  {struct, [erlang:list_to_tuple(Acc)]};
tuple_to_json(Tuple, CurrPos, Acc) ->
  Ele = prepare_for_json(element(CurrPos, Tuple)),
  tuple_to_json(Tuple, CurrPos - 1, [Ele|Acc]).

json_encode(Value) -> mochijson2:encode(Value).

% Turn query strings into proplists
% String = token=hsdhfhdf&big=bear
query_params_to_proplist(QueryString) ->
  Strings = string:tokens(QueryString, "&"),
  lists:flatten(lists:map(fun(Str) -> handle_single_query_key(Str, []) end, Strings)).

handle_single_query_key([$=|Rest], Acc) -> handle_single_query_value(Rest, lists:reverse(Acc), []);
handle_single_query_key([], _Acc) -> [];
handle_single_query_key([Chr|Rest], Acc) -> handle_single_query_key(Rest, [Chr|Acc]).

handle_single_query_value([], Key, Val) -> {misc_utils:to_atom(Key), misc_utils:to_list(lists:reverse(Val))};
handle_single_query_value([Chr|Rest], K, Val) -> handle_single_query_value(Rest, K, [Chr|Val]).
  

binify([]) -> misc_utils:to_bin("[]");
binify(List) ->
  case io_lib:char_list(List) of
    true -> misc_utils:to_bin(List);
    false -> binify1(List, [])
  end.

binify1([], Acc) -> lists:reverse(Acc);
binify1(Tuple, Acc) when is_tuple(Tuple) ->
  binify1([], [{struct, [binify2(Tuple)]}|Acc]);
binify1([Hd|Rest], Acc) ->
  binify1(Rest, [binify2(Hd)|Acc]).
  
binify2({Key, V}) ->
  case V of
    List when is_list(List) ->
      case io_lib:char_list(List) of
        true -> {Key, misc_utils:to_bin(List)};
        false -> 
          % check to see if the head is lists of strings
          case V of
            ListOfStrings when is_list(hd(ListOfStrings)) ->
              {Key, lists:map(fun(E) -> misc_utils:to_bin(E) end, V)};
            _Else ->
              {Key, {struct, binify(V)}}
          end
      end;
    Tuple when is_tuple(Tuple) -> {Key, binify(Tuple)};
    E -> {Key, misc_utils:to_bin(E)}
  end;
binify2(Ele) ->
  misc_utils:to_bin(Ele).

atom_to_binary(Val) ->
    <<_:4/binary, Bin/binary>> = term_to_binary(Val),
    Bin.