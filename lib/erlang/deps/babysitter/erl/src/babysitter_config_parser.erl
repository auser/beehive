-module(babysitter_config_parser).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(binary_to_list(Bin)).

parse(Input) ->
  setup_memo(),
  Result = case 'config_element'(Input,{{line,1},{column,1}}) of
             {AST, [], _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'config_element'(Input, Index) ->
  p(Input, Index, 'config_element', fun(I,D) -> (p_seq([p_optional(fun 'ws'/2), fun 'elem_list'/2, p_optional(fun 'ws'/2)]))(I,D) end, fun(Node, Idx) -> babysitter_list_utils:merge_proplists(Node) end).

'elem_list'(Input, Index) ->
  p(Input, Index, 'elem_list', fun(I,D) -> (p_seq([p_label('head', fun 'elem'/2), p_label('tail', p_zero_or_more(p_seq([p_optional(fun 'ws'/2), fun 'elem'/2])))]))(I,D) end, fun(Node, Idx) -> 
  case Node of
    [] -> [];
    [""] -> [];
    _ ->
      Head = proplists:get_value(head, Node),
      Tail = [R || [_,R] <- proplists:get_value(tail, Node)],
      [Head|Tail]
  end
 end).

'elem'(Input, Index) ->
  p(Input, Index, 'elem', fun(I,D) -> (p_choose([fun 'hook_elem'/2, fun 'action_elem'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'hook_elem'(Input, Index) ->
  p(Input, Index, 'hook_elem', fun(I,D) -> (p_seq([fun 'action'/2, p_string("."), p_choose([fun 'bef'/2, fun 'aft'/2]), p_string(":"), p_zero_or_more(p_charclass("[ \t]")), fun 'string'/2, p_choose([fun 'crlf'/2, p_not(p_anything())])]))(I,D) end, fun(Node, Idx) -> 
  {lists:nth(1, Node), 
    {lists:nth(3, Node), lists:flatten(lists:nth(6, Node))}
  }
 end).

'action_elem'(Input, Index) ->
  p(Input, Index, 'action_elem', fun(I,D) -> (p_seq([fun 'action'/2, p_string(":"), p_zero_or_more(p_charclass("[ \t]")), fun 'string'/2, p_choose([fun 'crlf'/2, p_not(p_anything())])]))(I,D) end, fun(Node, Idx) -> 
  {lists:nth(1, Node), 
    {command, lists:flatten(lists:nth(4, Node))}
  }
 end).

'action'(Input, Index) ->
  p(Input, Index, 'action', fun(I,D) -> (p_choose([p_string("bundle"), p_string("mount"), p_string("start"), p_string("stop"), p_string("unmount"), p_string("cleanup")]))(I,D) end, fun(Node, Idx) -> erlang:list_to_atom(lists:flatten(Node)) end).

'ws'(Input, Index) ->
  p(Input, Index, 'ws', fun(I,D) -> (p_zero_or_more(p_choose([fun 'comment'/2, fun 'space'/2])))(I,D) end, fun(Node, Idx) -> {} end).

'bef'(Input, Index) ->
  p(Input, Index, 'bef', fun(I,D) -> (p_string("before"))(I,D) end, fun(Node, Idx) -> pre end).

'aft'(Input, Index) ->
  p(Input, Index, 'aft', fun(I,D) -> (p_string("after"))(I,D) end, fun(Node, Idx) -> post end).

'string'(Input, Index) ->
  p(Input, Index, 'string', fun(I,D) -> (p_choose([fun 'bracketed_string'/2, fun 'nonbracketed_string'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'bracketed_string'(Input, Index) ->
  p(Input, Index, 'bracketed_string', fun(I,D) -> (p_seq([p_string("do"), p_label('str', p_zero_or_more(p_seq([p_not(p_string("end")), p_anything()]))), p_string("end")]))(I,D) end, fun(Node, Idx) -> proplists:get_value(str, Node) end).

'nonbracketed_string'(Input, Index) ->
  p(Input, Index, 'nonbracketed_string', fun(I,D) -> (p_zero_or_more(p_seq([p_not(fun 'crlf'/2), p_anything()])))(I,D) end, fun(Node, Idx) -> Node end).

'comment'(Input, Index) ->
  p(Input, Index, 'comment', fun(I,D) -> (p_seq([p_string("#"), p_zero_or_more(p_seq([p_not(fun 'crlf'/2), p_anything()])), fun 'crlf'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_charclass("[ \t\n\s\r]"))(I,D) end, fun(Node, Idx) -> Node end).

'crlf'(Input, Index) ->
  p(Input, Index, 'crlf', fun(I,D) -> (p_choose([p_string("\r\n"), p_string("\n")]))(I,D) end, fun(Node, Idx) -> Node end).






p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  % Grab the memo table from ets
  Memo = get_memo(StartIndex),
  % See if the current reduction is memoized
  case dict:find(Name, Memo) of
    % If it is, return the result
    {ok, Result} -> Result;
    % If not, attempt to parse
    _ ->
      case ParseFun(Inp, StartIndex) of
        % If it fails, memoize the failure
        {fail,_} = Failure ->
          memoize(StartIndex, dict:store(Name, Failure, Memo)),
          Failure;
        % If it passes, transform and memoize the result.
        {Result, InpRem, NewIndex} ->
          Transformed = TransformFun(Result, StartIndex),
          memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
          {Transformed, InpRem, NewIndex}
      end
  end.

setup_memo() ->
  put(parse_memo_table, ets:new(?MODULE, [set])).

release_memo() ->
  ets:delete(memo_table_name()).

memoize(Position, Struct) ->
  ets:insert(memo_table_name(), {Position, Struct}).

get_memo(Position) ->
  case ets:lookup(memo_table_name(), Position) of
    [] -> dict:new();
    [{Position, Dict}] -> Dict
  end.

memo_table_name() ->
    get(parse_memo_table).

p_eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

p_and(P) ->
  p_seq(P).

p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.

p_string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), p_advance_index(S,Index)};
        _ -> {fail, {expected, {string, S}, Index}}
      end
  end.

p_anything() ->
  fun([], Index) -> {fail, {expected, any_character, Index}};
     ([H|T], Index) -> {H, T, p_advance_index(H, Index)}
  end.

p_charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), p_advance_index(hd(Inp), Index)};
        _ -> {fail,{expected, {character_class, Class}, Index}}
      end
  end.

line({{line,L},_}) -> L;
line(_) -> undefined.

column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) -> % strings
  lists:foldl(fun p_advance_index/2, Index, MatchedInput);
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
