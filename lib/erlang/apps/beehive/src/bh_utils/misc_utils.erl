-module (misc_utils).
-include ("common.hrl").
-compile (export_all).

-compile({no_auto_import, [max/2]}).

-define (ADJECTIVES, [
  "fast", "quick", "clean", "positive", "generous", "silly", "enjoyable", "friendly", "flighty", "handsome", "hot", "adorable", "cool", "cold", "odd"
]).

-define (NOUNS, [
  "giant", "bee", "queenbee", "sound", "music", "honey", "smile", "balloon", "bird", "wind", "dog", "cat", "duck", "moose", "fish", "guitar", "sparrow", "buzz"
]).

-define (LETTERS, "abcdefghijklmnopqrstuvwxyz0123456789").

generate_unique_name(Name, Num) -> 
  lists:flatten([
    Name, "-", random_word(?LETTERS, Num, [])
  ]).
generate_unique_name(Num) when is_integer(Num) ->
  lists:flatten([
    random_word(?ADJECTIVES),"-",
    random_word(?NOUNS),"-",
    random_word(?LETTERS, Num, [])
  ]).

random_word(_, 0, Acc) -> lists:reverse(Acc);
random_word(List, Length, Acc) ->
  random_word(List, Length - 1, [random_word(List)|Acc]).
  
random_word(List) ->
  lists:nth(random:uniform(erlang:length(List)), List).


%% clean up the tempfile, if necessary
cleanup_tempfile(Tempfile) ->
  case filelib:is_file(Tempfile) of
    true -> file:delete(Tempfile);
    false -> ok
  end.

to_list(Bin) when is_binary(Bin)    -> erlang:binary_to_list(Bin);
to_list(Atom) when is_atom(Atom)    -> erlang:atom_to_list(Atom);
to_list(Float) when is_float(Float) -> erlang:float_to_list(Float);
to_list(Int) when is_integer(Int)   -> erlang:integer_to_list(Int);
to_list(List)                       -> List.

to_atom(Bin) when is_binary(Bin) -> to_atom(erlang:binary_to_list(Bin));
to_atom(List) when is_list(List) -> erlang:list_to_atom(List);
to_atom(Atom) -> Atom.

to_bin(undefined)                  -> <<"undefined">>;
to_bin(Tuple) when is_tuple(Tuple) -> to_bin(erlang:term_to_binary(Tuple));
to_bin(Atom) when is_atom(Atom)    -> to_bin(erlang:atom_to_list(Atom));
to_bin(Int) when is_integer(Int)   -> to_bin(erlang:integer_to_list(Int));
to_bin(Float) when is_float(Float) -> to_bin(erlang:float_to_list(Float));
to_bin(List) when is_list(List)    -> 
  case io_lib:char_list(List) of
    true -> erlang:list_to_binary(List);
    false -> lists:map(fun(Ele) -> to_bin(Ele) end, List)
  end;
to_bin(Bin)                        -> Bin.

to_integer(Str) when is_list(Str) -> erlang:list_to_integer(Str);
to_integer(Int) -> Int.

max([])                  -> 0;
max([H|T])               -> max(H, T).
max(M, [])               -> M;
max(M, [H|L]) when M > H -> max(M, L);
max(_M, [H|L])           -> max(H,L).

update_proplist(OldProplist, []) -> lists:reverse(OldProplist);
update_proplist(OldProplist, [{Key, Value}|Rest]) ->
  case proplists:is_defined(Key, OldProplist) of
    true -> 
      OldProplist2 = proplists:delete(Key, OldProplist),
      update_proplist([{Key, Value}|OldProplist2], Rest);
    false ->
      update_proplist([{Key, Value}|OldProplist], Rest)
  end.


%% Only choose values that are actually in the proplist
filter_proplist(_Proplist, [], Acc) -> Acc;
filter_proplist(Proplist, [{K,V}|Rest], Acc) ->
  case proplists:is_defined(K, Proplist) of
    false -> filter_proplist(Proplist, Rest, Acc);
    true -> filter_proplist(Proplist, Rest, [{K,V}|Acc])
  end.

new_or_previous_value(_NewProplist, [], Acc) -> Acc;
new_or_previous_value(NewProplist, [{K,V}|Rest], Acc) ->
  case proplists:is_defined(K,NewProplist) of
    true -> 
      NewV = proplists:get_value(K, NewProplist),
      new_or_previous_value(NewProplist, Rest, [{K, NewV}|Acc]);
    false ->
      new_or_previous_value(NewProplist, Rest, [{K, V}|Acc])
  end.
  
%% Merge the two proplists, so that if there are overlapping values,
%% make them be appended
proplist_merge(undefined, B) -> B;
proplist_merge(A, undefined) -> A;
proplist_merge(A, B) -> proplist_merge1(A, B, []).
proplist_merge1([], [], Acc) -> lists:reverse(Acc);
proplist_merge1([], [{K,_V}=Tuple|Rest], Acc) ->
  case proplists:get_value(K, Acc) of
    undefined -> proplist_merge1([], Rest, [Tuple|Acc]);
    _Else -> proplist_merge1([], Rest, Acc)
  end;
proplist_merge1([{K,V}=Tuple|Rest], B, Acc) ->
  case proplists:get_value(K, B) of
    undefined -> proplist_merge1(Rest, B, [Tuple|Acc]);
    Else -> proplist_merge1(Rest, lists:delete(K, B), [{K, proplist_merge_helper(V, Else)}|Acc])
  end.

proplist_merge_helper(List, Other) ->
  case io_lib:char_list(List) of
    true ->
      lists:append([[List], [Other]]);
    _ ->
      lists:append([List|Other])
  end.

%% name of the local node
localnode(Name) ->
  list_to_atom(lists:append([atom_to_list(Name), "@", nodehost(node())])).

%% Get the name of the local node
nodehost(Node) -> tl(lists:dropwhile(fun(E) -> E =/= $@ end, atom_to_list(Node))).
nodename(Node) -> lists:takewhile(fun(Chr) -> Chr =/= $@ end, atom_to_list(Node)).

%% Turn the proplists into atoms
atomize([], Acc) -> Acc;
atomize([{K,V}|Rest], Acc) -> atomize(Rest, [{misc_utils:to_atom(K), V}|Acc]).

%% Reload all the beehive modules
reload_code() ->
  F = fun(M) -> 
    code:purge(M),
    code:soft_purge(M),
    {module, M} = code:load_file(M),
    {ok, M}
  end,
  [F(M) || {M,P} <- code:all_loaded(), is_list(P) andalso string:str(P, "beehive") > 0 ].

%% Reload code on all attached nodes
reload_all() ->
  reload_code(),
  rpc:multicall(nodes(), misc_utils, reload_code, [], timer:seconds(30)),
  node_manager:read_babysitter_config().


