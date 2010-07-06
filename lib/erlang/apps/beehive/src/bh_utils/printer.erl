-module (printer).

-export ([banner/1, banner/2]).
-define (HEADER_CHAR, $-).

banner(Header, Lines) ->
  DescrLen = lists:max([length(L) || L <- Lines]),
  divider(Header, DescrLen),
  lists:foreach(fun(L) ->
    io:format(" ~-" ++ integer_to_list(DescrLen) ++ "s~n", [L])
  end, Lines),
  divider(DescrLen + length(Header) - 2).
  
  
banner(Lines) ->
  DescrLen = lists:max([length(K) + length(V) || {K, V} <- Lines]),
  LongestHeader = lists:max([length(K) || {K, _V} <- Lines]) + 1,
  [H|Rest] = Lines,
  divider(H, DescrLen),
  lists:foreach(fun (L) ->
    case L of
      {K,V} -> 
        io:format("~-" ++ integer_to_list(LongestHeader) ++ "s: ~s~n", [K,V]);
      Line ->
        io:format("~-" ++ integer_to_list(DescrLen) ++ "~s~n", [Line])
    end
  end, Rest),
  divider(DescrLen + length(H)).
  
divider(Header, DescrLen) when is_list(Header) ->
  Size = erlang:round((DescrLen - 2)/2),
  DupChars = lists:duplicate(Size, ?HEADER_CHAR),
  io:format("~s ~s ~s~n", [DupChars, Header, DupChars]).

divider(DescrLen) when is_integer(DescrLen) ->
  DupChars = lists:duplicate((DescrLen + 2), ?HEADER_CHAR),
  io:format("~s~n", [DupChars]).