%%% babysitter_config_srv.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 04/22/10 Ari Lerner <arilerner@mac.com>
%% @doc The babysitter configuration
-module (babysitter_config).
-include ("babysitter.hrl").

-export ([
  init/0,
  read/1,
  get/2,
  get_raw_config/1
]).

-define (CONF_EXTENSION, ".conf").

%%-------------------------------------------------------------------
%% @spec () ->    {ok, Value}
%% @doc Init the config
%%      
%% @end
%%-------------------------------------------------------------------
init() ->
  case ets:info(?BABYSITTER_CONFIG_DB) of
    undefined -> ets:new(?BABYSITTER_CONFIG_DB, [set, named_table, protected]);
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
  case filelib:is_dir(Dir) of
    true -> read_dir(Dir);
    false ->
      case filelib:is_file(Dir) of
        true -> read_files([Dir], []);
        false -> throw({badarg, "Argument must be a file or a directory"})
      end
  end.

%%-------------------------------------------------------------------
%% @spec (Type::string(), Action::string()) ->    {ok, Value}
%%                                              | {error, no_action}
%%                                              | {error, not_found}
%% @doc Fetch a configuration value from the ets table
%%      
%% @end
%%-------------------------------------------------------------------
get(Type, Action) ->
  case get_raw_config(Type) of
    {ok, {Type, Proplists}} -> 
      case proplists:get_value(Action, Proplists) of
        undefined -> {error, no_action};
        V -> {ok, V}
      end;
    _Else -> {error, not_found}
  end.

%%-------------------------------------------------------------------
%% @spec (State::string()) ->    {ok, Value}
%% @doc Get the raw stored configuration
%%      
%% @end
%%-------------------------------------------------------------------
get_raw_config(State) ->
  case ets:lookup(?BABYSITTER_CONFIG_DB, State) of
    [Else] when is_tuple(Else) -> {ok, Else};
    _E -> {error, not_found}
  end.

%%-------------------------------------------------------------------
%% @spec (Dir::list()) -> {ok, Files::list()}
%% @doc Read a directory and parse the the conf files
%%      
%% @end
%%-------------------------------------------------------------------
read_dir(Dir) ->
  Files = lists:filter(fun(F) -> filelib:is_file(F) end, filelib:wildcard(lists:flatten([Dir, "/*", ?CONF_EXTENSION]))),
  read_files(Files, []).

%%-------------------------------------------------------------------
%% @spec (Files::list(), Acc::list()) -> {ok, Files}
%% @doc Take a list of files and parse them into the config db
%%      
%% @end
%%-------------------------------------------------------------------
read_files([], Acc) -> {ok, lists:reverse(Acc)};
read_files([File|Rest], Acc) ->
  {Filename, Config} = parse_config_file(File),
  init(),
  ets:insert(?BABYSITTER_CONFIG_DB, [{Filename, Config}]),
  read_files(Rest, [Filename|Acc]).

%%-------------------------------------------------------------------
%% @spec (Filepath::string()) ->    ok
%%                                | {error, Reason}
%% @doc Parse the file at Filepath
%%      
%% @end
%%-------------------------------------------------------------------
parse_config_file(Filepath) ->
  case babysitter_config_parser:file(Filepath) of
    {fail, Reason} -> {error, Reason};
    Else ->
      Config = fill_record_from_proplist(Else, []),
      Filename = erlang:list_to_atom(filename:basename(Filepath, ?CONF_EXTENSION)),
      {Filename, Config}
  end.

%%-------------------------------------------------------------------
%% @spec (Proplist::list()) -> record()
%% @doc Take a proplist and fill a config_rec with the
%%      actions from the proplist
%% @end
%%-------------------------------------------------------------------
fill_record_from_proplist([], Record) -> Record;
fill_record_from_proplist([{Key, Value}|Rest], Record) ->
  ActionRec = extract_into_action_rec(Value),
  NewConfigRec = [{Key, ActionRec}|Record],
  fill_record_from_proplist(Rest, NewConfigRec).
  
extract_into_action_rec(Proplist) ->
  Before = proplists:get_value(pre, Proplist),
  Command = proplists:get_value(command, Proplist),
  After = proplists:get_value(post, Proplist),
  {Before, Command, After}.