%%%-------------------------------------------------------------------
%%% File    : bh_file_utils.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Mon Nov 16 14:09:53 PST 2009
%%%-------------------------------------------------------------------

-module (bh_file_utils).
-include ("common.hrl").
-include_lib("kernel/include/file.hrl").
-compile (export_all).

root_dir(Path) ->
  filename:join([?BEEHIVE_HOME, Path]).

find_git_root("/") -> {error, no_git_root};
find_git_root(Path) ->
  case filelib:is_dir(filename:join([Path, ".git"])) of
    true -> Path;
    false ->
      Split = filename:split(Path),
      NewPath = lists:sublist(Split,1,length(Split)-1),
      find_git_root(filename:join(NewPath))
  end.

relative_or_abs_path(List) ->
  case abs_or_relative_filepath(List) of
    true -> List;
    _ ->
      case filelib:is_file(F = filename:join([?BEEHIVE_HOME, List])) of
        true  -> F;
        false ->
          lists:flatten([?BH_ROOT, "/", List])
      end
  end.

%% Find a file either absolute or relative
abs_or_relative_filepath(P) ->
  case filelib:is_file(P) of
    true -> P;
    false ->
      case filelib:is_file(relative_path(P)) of
        true -> relative_path(P);
        false -> {error, not_a_file}
      end
  end.

%% Get the relative path joined with the filename
relative_path(P) ->
  filename:join([filename:absname(""), P]).

ensure_dir_exists([]) -> ok;
ensure_dir_exists([Dir|Rest]) ->
  filelib:ensure_dir(Dir ++ "/.nonexistant_file"),
  ensure_dir_exists(Rest).

is_symlink(Path) ->
  case file:read_link_info(Path) of
    {ok, #file_info{type = symlink}} -> true;
    _ -> false
  end.

file_type(Path) ->
  case filelib:is_regular(Path) of
    true -> file;
    false ->
      case is_symlink(Path) of
        true -> symlink;
        false -> directory
      end
  end.

walk(Path, Level, Fun) ->
  FileType = file_type(Path),
  case FileType of
    file ->
      Fun({file, Level, Path});
    symlink -> Fun({symlink, Level, Path});
    directory ->
      Children = filelib:wildcard(filename:join([Path, "*"])),
      case Children of
        [] -> ok;
        _ -> lists:foreach(fun(P) -> walk(P, Level + 1, Fun) end, Children)
      end,
      Fun({directory, Level, Path})
  end.

%% DELETE ALL
rm_rf(Dir) ->
  walk(Dir, 0, fun({Type, _Level, Path}) ->
                   case Type of
                     directory -> file:del_dir(Path);
                     _ -> file:delete(Path)
                   end
               end),
  file:del_dir(Dir).
