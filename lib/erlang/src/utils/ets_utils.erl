-module (ets_utils).
-compile (export_all).

to_key(List) when is_list(List) -> erlang:list_to_atom(List);
to_key(K) -> K.