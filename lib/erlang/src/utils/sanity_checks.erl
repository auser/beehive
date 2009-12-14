%%%-------------------------------------------------------------------
%%% File    : sanity_checks.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:15:35 PST 2009
%%%-------------------------------------------------------------------

-module (sanity_checks).

-export ([check/1]).

check(node) ->
  check_db_directory(),
  ok;
check(storage) ->
  check_db_directory(),
  ok;
check(router) ->
  io:format("checking router...~n~n"),
  % Check the db directory, make sure it exists and is writeable
  check_db_directory(),
  ok.
  
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
