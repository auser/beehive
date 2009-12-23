%%%-------------------------------------------------------------------
%%% File    : bh_file_utils.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Nov 16 14:09:53 PST 2009
%%%-------------------------------------------------------------------

-module (bh_file_utils).

-compile (export_all).

% Find a file either absolute or relative
abs_or_relative_filepath(P) ->
  case filelib:is_file(P) of
    true -> P;
    false ->
      case filelib:is_file(relative_path(P)) of
        true -> relative_path(P);
        false -> throw({error, not_a_file})
      end
  end.

% Get the relative path joined with the filename
relative_path(P) ->
  filename:join([filename:absname(""), P]).