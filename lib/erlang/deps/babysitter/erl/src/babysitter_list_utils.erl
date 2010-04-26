-module (babysitter_list_utils).

-export ([merge_proplists/1]).

%%-------------------------------------------------------------------
%% @spec (Proplists::list()) -> NewProplist
%% @doc Take a list of proplists that contain the same keys and merge them together.
%%      For example, take a proplist that looks like:
%%
%%      [{bundle,{command,"echo Bundle java stuff"}},{bundle,{before,"echo Before bundle"}},{bundle,{aft,"echo After bundle"}}]
%%
%%      and convert it to this:
%%
%%      {bundle,[{command,"echo Bundle java stuff"},{before,"echo Before bundle"},{aft,"echo After bundle"}]}]
%%
%% @end
%%-------------------------------------------------------------------

merge_proplists(Proplists) -> merge_proplists(Proplists, []).
merge_proplists([], Acc) -> lists:reverse(Acc);
merge_proplists([{Key, Value}|Rest], Acc) ->
  M = case proplists:get_value(Key, Acc) of
    undefined -> [{Key, [Value]}|Acc];
    V ->
      [{Key, lists:flatten([V, Value])}|proplists:delete(Key, Acc)]
  end,
  merge_proplists(Rest, M);
merge_proplists([List|Rest], Acc) when is_list(List) -> merge_proplists(Rest, merge_proplists(List, Acc));
merge_proplists([_Other|Rest], Acc) -> 
  merge_proplists(Rest, Acc).
