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
