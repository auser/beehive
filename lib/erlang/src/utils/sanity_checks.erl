%%%-------------------------------------------------------------------
%%% File    : sanity_checks.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:15:35 PST 2009
%%%-------------------------------------------------------------------

-module (sanity_checks).

-export ([check/1]).

check(bee) -> check_for_all();
check(storage) -> check_for_all();
check(router) -> check_for_all().

check_for_all() ->
  check_db_directory().
  
% Internals
check_db_directory() ->
  case db:info(directory) of
    undefined -> ok;
    Dir ->
      case filelib:is_dir(Dir) of
        true -> ok;
        false -> erlang:throw({db, directory, no_exists})
      end,
      case writeable(Dir) of
        true -> ok;
        false -> erlang:throw({db, directory, not_writeable})
      end
  end.
  
writeable(F) ->
    case file:read_file_info(F) of
      {ok, Tuple} when element(4, Tuple) =:= read_write -> true;
      {ok, Tuple} when element(4, Tuple) =:= write -> true;
      _ -> false
    end.
