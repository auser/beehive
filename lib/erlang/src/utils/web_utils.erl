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

binify(Proplist) ->
  lists:map(fun({Key, V}) ->
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
    end
  end, Proplist).
