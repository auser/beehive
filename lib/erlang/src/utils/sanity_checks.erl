%%%-------------------------------------------------------------------
%%% File    : sanity_checks.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:15:35 PST 2009
%%%-------------------------------------------------------------------

-module (sanity_checks).

-export ([check/1]).

check(node) -> check_for_all();
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
        false -> 
          % If the directory doesn't exist yet, let's create it
          filelib:ensure_dir(Dir)
          % erlang:throw(error, {db_directory, no_exists, Dir})
      end
      % case writeable(Dir) of
      %   true -> ok;
      %   false -> erlang:throw(error, {db_directory, not_writeable, Dir})
      % end
  end.
  
% writeable(F) ->
%     case file:read_file_info(F) of
%       {ok, Tuple} when element(4, Tuple) =:= read_write -> true;
%       {ok, Tuple} when element(4, Tuple) =:= write -> true;
%       _ -> false
%     end.
