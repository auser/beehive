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

binify([]) -> misc_utils:to_bin("[]");
binify(List) ->
  case io_lib:char_list(List) of
    true -> misc_utils:to_bin(List);
    false -> binify1(List, [])
  end.

binify1([], Acc) -> Acc;
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