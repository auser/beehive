%%% beehive_bee_object_config_srv.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 04/22/10 Ari Lerner <arilerner@mac.com>
%% @doc The babysitter configuration
-module (beehive_bee_object_config).
-include ("beehive.hrl").

-export ([
  init/0,
  read/1,
  get/2, get_or_default/2,
  get_raw_config/1,
  list_configs/0, list_actions/1
]).

-define (CONF_EXTENSION, ".sh").
-define (BEEHIVE_BEE_OBJECT_CONFIG_DB, 'beehive_bee_object_config_db').

%%-------------------------------------------------------------------
%% @spec () ->    {ok, Value}
%% @doc Init the config
%%
%% @end
%%-------------------------------------------------------------------
init() ->
  case catch ets:info(?BEEHIVE_BEE_OBJECT_CONFIG_DB) of
    undefined -> ets:new(?BEEHIVE_BEE_OBJECT_CONFIG_DB,
                         [set, named_table, public]);
    _ -> ok
  end.

%%-------------------------------------------------------------------
%% @spec (Dir::list()) -> {ok, FileNames}
%% @doc Take a directory of files and read and parse them into the
%%      ets database.
%% @end
%%-------------------------------------------------------------------
read(Dir) ->
  init(),
  lists:foreach(fun(F) ->
    case filelib:is_dir(F) of
      true -> read_dir(F);
      false -> ok
    end
  end, filelib:wildcard(lists:flatten([Dir, "/*"]))).

get_or_default(Type, Key) ->
  case get(Type, Key) of
    {error, _} ->
      case get_raw_config(Type) of
        {error, _} = T2 -> T2;
        {ok, Dict} -> dict:fetch(default, Dict)
      end;
    E -> E
  end.

%%-------------------------------------------------------------------
%% @spec (Type::string(), Action::string()) ->    {ok, Value}
%%                                              | {error, no_action}
%%                                              | {error, not_found}
%% @doc Fetch a configuration value from the ets table
%%
%% @end
%%-------------------------------------------------------------------
get(Type, Key) ->
  case get_raw_config(Type) of
    {error, _} = T -> T;
    {ok, Dict} ->
      case dict:is_key(Key, Dict) of
        true -> dict:fetch(Key, Dict);
        false -> {error, not_found}
      end
  end.

%%-------------------------------------------------------------------
%% @spec () ->    List::list()
%% @doc List all the config
%%
%% @end
%%-------------------------------------------------------------------
list_configs() ->
  List = ets:tab2list(?BEEHIVE_BEE_OBJECT_CONFIG_DB),
  lists:map(fun({Name, _Config}) -> Name end, List).

%%-------------------------------------------------------------------
%% @spec (Type) ->    List::list()
%% @doc
%%
%% @end
%%-------------------------------------------------------------------
list_actions(Type) ->
  case get_raw_config(Type) of
    {ok, Dict} -> dict:fetch_keys(Dict);
    _ -> {error, not_found}
  end.

%%-------------------------------------------------------------------
%% @spec (State::string()) ->    {ok, Value}
%% @doc Get the raw stored dict
%%
%% @end
%%-------------------------------------------------------------------
get_raw_config(Type) ->
  case ets:lookup(?BEEHIVE_BEE_OBJECT_CONFIG_DB, Type) of
    [{Type, Dict}|_] -> {ok, Dict};
    _E -> {error, not_found}
  end.

%%-------------------------------------------------------------------
%% @spec (Dir::list()) -> {ok, Files::list()}
%% @doc Read a directory and parse the the conf files
%%
%% @end
%%-------------------------------------------------------------------
read_dir(Dir) ->
  Dirname = list_to_atom(filename:basename(Dir)),
  FoundDict = case ets:lookup(?BEEHIVE_BEE_OBJECT_CONFIG_DB, Dirname) of
    [{Dirname, Dict}] -> Dict;
    _E -> dict:new()
  end,
  Files = lists:filter(fun(F) -> filelib:is_file(F) end,
                       filelib:wildcard(
                         lists:flatten([Dir, "/*", ?CONF_EXTENSION]))),
  {ok, NewDict} = read_files(FoundDict, Files),
  ets:insert(?BEEHIVE_BEE_OBJECT_CONFIG_DB, [{Dirname, NewDict}]).

%%-------------------------------------------------------------------
%% @spec (Files::list(), Acc::list()) -> {ok, Files}
%% @doc Take a list of files and parse them into the config db
%%
%% @end
%%-------------------------------------------------------------------
read_files(Dict, []) -> {ok, Dict};
read_files(Dict, [File|Rest]) ->
  {Filename, Config} = read_config_file(File),
  init(),
  Type = list_to_atom(
           string:sub_string(Filename, 1,
                             length(Filename) - length(filename:extension(Filename)))),
  read_files(dict:store(Type, Config, Dict), Rest).

%%-------------------------------------------------------------------
%% @spec (Filepath::string()) ->    ok
%%                                | {error, Reason}
%% @doc Parse the file at Filepath
%%
%% @end
%%-------------------------------------------------------------------
read_config_file(Filepath) ->
  case filelib:is_file(Filepath) of
    false -> {error, {unknown_type, Filepath}};
    true ->
      {ok, Binary} = file:read_file(Filepath),
      Lines = string:tokens(binary_to_list(Binary), "\n"),
      {filename:basename(Filepath), string:join(Lines, "\n")}
  end.
