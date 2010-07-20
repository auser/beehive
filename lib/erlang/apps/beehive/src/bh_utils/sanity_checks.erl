%%%-------------------------------------------------------------------
%%% File    : sanity_checks.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Nov 29 23:15:35 PST 2009
%%%-------------------------------------------------------------------

-module (sanity_checks).
-include ("common.hrl").

-export ([check/1]).
-define (DB, beehive_db_srv).

check(test_type) -> ok;
check(beehive_node) -> check_for_all();
check(beehive_storage) -> check_for_all();
check(beehive_router) -> check_for_all().

check_for_all() ->
  check_db_directory().

% Internals
check_db_directory() ->
  case ?DB:info(directory) of
    undefined -> ok;
    Dir ->
      case filelib:is_dir(Dir) of
        true -> ok;
        false -> 
          % If the directory doesn't exist yet, let's create it
          case bh_file_utils:ensure_dir_exists([Dir]) of
            ok -> ok;
            _ -> 
              MnesiaDir = filename:join(?BEEHIVE_HOME, "db"),
              application:set_env(mnesia, dir, MnesiaDir)
            % erlang:throw({error, {db_directory, no_exists, Dir}})
          end
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
