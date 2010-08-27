%%% beehive_bee_object.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 07/23/10 Ari Lerner <arilerner@mac.com>
%% @doc
-module (beehive_bee_object).
-include_lib("kernel/include/file.hrl").
-include ("beehive.hrl").
-include ("common.hrl").

-export ([
  init/0,
  % Actions
  clone/1,clone/2,
  bundle/1,bundle/2,
  mount/2, mount/3,
  start/3, start/4,
  stop/1, stop/2,
  unmount/2, unmount/3,
  have_bee/1,
  info/1,
  cleanup/1, cleanup/2,
  ls/1, ls/0
]).

% Transportation
-export ([
  send_bee_object/2, send_bee_object/3,
  get_bee_object/2,
  save_bee_object/2
]).


-define (DEBUG, false).
-define (DEBUG_PRINT (Args), fun() ->
  case ?DEBUG of
    true -> erlang:display(Args);
    false -> ok
  end
end()).

-define (DEBUG_RM (Dir), fun() ->
  case ?DEBUG of
    false -> rm_rf(Dir);
    true -> ok
  end
end()).

-define (BEEHIVE_BEE_OBJECT_INFO_TABLE, 'beehive_bee_object_info').
-define (BEEHIVE_BEE_OBJECT_INFO_TABLE_PROCESS, 'beehive_bee_object_info_process').

% ETS functions
-export ([ets_process_restarter/0, start_ets_process/0]).

% Initialize included bee_tpes
init() ->
  beehive_ets_helper:spawn_and_monitor(?BEEHIVE_BEE_OBJECT_INFO_TABLE_PROCESS, [?BEEHIVE_BEE_OBJECT_INFO_TABLE]),
  % Ewww
  Dir =?BH_ROOT,
  beehive_bee_object_config:init(),
  beehive_bee_object_config:read(filename:join([Dir, "etc", "app_templates"])).

% List the bees in the directory
ls() -> ls(config:search_for_application_value(squashed_dir, ?BEEHIVE_DIR("squashed"))).
ls(BeeDir) ->
  lists:map(fun(Filepath) ->
    Filename = filename:basename(Filepath),
    string:sub_string(Filename, 1, length(Filename) - length(".bee"))
  end, filelib:wildcard(filename:join([BeeDir, "*.bee"]))).

% Create a new object from a directory or from a url
% This will clone the repository and ensure that the given
% revision matches the actual revision of the repository
clone(E) -> clone(E, undefined).
clone(GivenProplist, From) when is_list(GivenProplist) ->
  BeeObject = from_proplists(GivenProplist),
  clone(BeeObject, From);

clone(#bee_object{template=Type, bundle_dir=BundleDir, revision=Rev}=BeeObject, From) when is_record(BeeObject, bee_object) ->
  case beehive_bee_object_config:get_or_default(clone, Type) of
    {error, Reason} -> {error, {clone, Reason}};
    AfterClone ->
    % Cleanup before...
    ?DEBUG_RM(BundleDir),
    % Run before, if it needs to run
    run_hook_action(pre, BeeObject, From),

    case ensure_repos_exists(BeeObject, From) of
      {error, _Reason} = T2 ->
        ?DEBUG_PRINT({error, ensure_repos_exists, T2}),
        T2;
      Out ->
        case Rev of
          undefined -> ok;
          _ -> ensure_repos_is_current_repos(BeeObject)
        end,
        TDude = run_in_directory_with_file(BeeObject, From, BundleDir, AfterClone),
        ?DEBUG_PRINT({run_in_directory_with_file, TDude, Out}),
        run_hook_action(post, BeeObject, From),
        Out
    end
  end.

% Squash the bee object
% This will clone an object based on a given type and a bee_type
% configuration file
% It will run the configuration file code first and then
% bundle it using a temp file
bundle(E) -> bundle(E, undefined).

bundle(Proplists, From) when is_list(Proplists) ->
  BeeObject = from_proplists(Proplists),
  bundle(BeeObject, From);

% Take a url and clone/1 it and then bundle the directory
% based on the configuration directive
bundle(#bee_object{template=Type, bundle_dir=NBundleDir} = BeeObject, From) when is_record(BeeObject, bee_object) ->
  case clone(BeeObject#bee_object{pre = undefined, post = undefined}, From) of
    {error, {_ExitStatus, _Reason}} = CloneError ->
      % Cleanup and send error
      ?DEBUG_RM(NBundleDir),
      send_to(From, CloneError),
      CloneError;
    _E ->
      _BundleDir = filename:dirname(NBundleDir),
      run_hook_action(pre, BeeObject, From),
      case beehive_bee_object_config:get_or_default(bundle, Type) of
        {error, Reason} -> {error, {bundle, Reason}};
        BeforeBundle ->
          % Run the bundle pre config first, then the bundle command
          case run_in_directory_with_file(BeeObject, From, NBundleDir, BeforeBundle) of
            {error, _} = T2 -> T2;
            _BeforeActionOut ->
              SquashCmd = proplists:get_value(bundle, config_props()),
              Proplist = to_proplist(BeeObject),
              Str = template_command_string(SquashCmd, Proplist),

              cmd(Str, NBundleDir, Proplist, From),

              run_hook_action(post, BeeObject, From),
              ?DEBUG_RM(NBundleDir),
              write_info_about_bee(BeeObject),
              info(BeeObject)
          end
      end
  end.

write_info_about_bee(#bee_object{
                        bee_file = BeeFile,
                        name = Name} = BeeObject) ->

  Dict = case ets:lookup(?BEEHIVE_BEE_OBJECT_INFO_TABLE, Name) of
    [] -> dict:new();
    [{Name, D}|_] -> D
  end,

  {ok, Fileinfo} = file:read_file_info(BeeFile),
  CheckedRev = case get_current_sha(BeeObject) of
    {ok, Rev} -> Rev;
    _ -> "HEAD"
  end,

  Info =  [{revision, CheckedRev}, {bee_size, Fileinfo#file_info.size},  {created_at, calendar:datetime_to_gregorian_seconds(Fileinfo#file_info.ctime)}|to_proplist(BeeObject)],

  NewDict = lists:foldl(fun({K,V}, Acc) ->
    case V of
      undefined -> Acc;
      _ -> dict:store(K,V, Acc)
    end
  end,
  Dict, Info),

  ets:insert(?BEEHIVE_BEE_OBJECT_INFO_TABLE, [{Name, NewDict}]),
  dict:to_list(NewDict).

% Mount the bee
mount(Type, Name) -> mount(Type, Name, undefined).
mount(Type, Name, From) ->
  case beehive_bee_object_config:get_or_default(mount, Type) of
    {error, _} = T ->
      send_to(From, T),
      throw(T);
    AfterMountScript ->
      BeeFile = find_bee_file(Name),
      MountRootDir = config:search_for_application_value(run_dir, ?BEEHIVE_DIR("run")),
      MountDir = filename:join([MountRootDir, Name]),
      MountCmd = proplists:get_value(mount, config_props()),

      % I *think* this should happen here
      ?DEBUG_RM(MountDir),
      BeeObject = from_proplists([{name, Name}, {template, Type}, {bee_file, BeeFile},
                                  {run_dir, MountDir}, {bundle_dir, filename:dirname(BeeFile)}
                                ]),

      run_hook_action(pre, BeeObject, From),
      Str = template_command_string(MountCmd, to_proplist(BeeObject)),
      ?DEBUG_PRINT({mount,run_dir,MountDir,Str}),
      ok = ensure_directory_exists(filename:join([MountDir, "dummy_dir"])),
      case run_command_in_directory(Str, MountDir, From, BeeObject) of
        {error, _Reason} = T1 -> T1;
        T2 ->
          run_in_directory_with_file(BeeObject, From, MountDir, AfterMountScript),
          run_hook_action(post, BeeObject, From),
          T2
    end
  end.

% Start the beefile
start(Type, Name, Port) -> start(Type, Name, Port, undefined).
start(Type, Name, Port, From) ->
  case beehive_bee_object_config:get_or_default(start, Type) of
    {error, _} = T -> throw(T);
    StartScript ->
      mount(Type, Name),
      BeeDir = find_mounted_bee(Name),
      FoundBeeObject = find_bee(Name),
      Self = self(),
      BeeObject = FoundBeeObject#bee_object{port = Port, run_dir = BeeDir},

      {ok, PidFilename, PidIo} = temp_file(),
      file:close(PidIo),
      % BeeObject = from_proplists([{name, Name}, {template, Type}, {bee_file, BeeFile}, {run_dir, BeeDir}, {port, Port}]),
      CmdProcessPid = spawn(fun() ->
        {ok, ScriptFilename, ScriptIo} = temp_file(),
        file:write(ScriptIo, StartScript),
        try
          {Pid, Ref, Tag} =
            async_command("/bin/sh",
                          [ScriptFilename],
                          BeeDir,
                          [{pidfile, PidFilename}|to_proplist(BeeObject)],
                          From),
          % Hardcoded delays... eww
          timer:sleep(500),
          OsPid = case read_pid_file_or_retry(PidFilename, 500) of
            {error, _} ->
              timer:sleep(100),
              read_pid_file_or_retry(PidFilename, 500);
            PidInt -> PidInt
          end,
          file:delete(ScriptFilename),
          file:delete(PidFilename),

          RealBeeObject = BeeObject#bee_object{os_pid = OsPid, template = Type},
          write_info_about_bee(RealBeeObject),
          send_to(Self, {started_bee_object, RealBeeObject}),

          cmd_receive(Pid, [], From, fun(Msg) ->
            case Msg of
              {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
              {'DOWN', Ref, process, Pid, Reason} ->
                send_to(From, {stopped, {Name, Reason}});
              {stop, Caller} ->
                ?DEBUG_PRINT({cmd_received,{stop, Caller}, OsPid}),
                case OsPid of
                  IntPid when is_integer(IntPid) andalso IntPid > 1 ->
                    run_kill_on_pid(OsPid, BeeDir, RealBeeObject),
                    send_to(Caller, {stopped, RealBeeObject});
                  _ -> ok
                end;
              _E ->
                % run_kill_on_pid(OsPid, BeeDir, RealBeeObject),
                ok
            end
          end)
        after
          % Just in case
          file:delete(ScriptFilename)
        end
      end),
      F = fun(This) ->
        receive
          {started_bee_object, SentRealBeeObject} ->
            From ! {started, SentRealBeeObject#bee_object{pid = CmdProcessPid}};
          _X -> This(This)
        end
      end,
      F(F)
  end.

stop(Name) -> stop(Name, undefined).
stop(#bee{pid = Pid} = Bee, From) when is_record(Bee, bee) ->
  stop(Pid, From);
stop(#bee_object{pid = Pid} = BeeObject, From) when is_record(BeeObject, bee_object) ->
  stop(Pid, From);
stop(Pid, From) when is_pid(Pid) ->
  send_to(Pid, {stop, self()}),
  receive
    {stopped, #bee_object{name = Name} = RealBeeObject} ->
      case ets:lookup(?BEEHIVE_BEE_OBJECT_INFO_TABLE, Name) of
        [] -> ok;
        [{Name, _D} = T|_] -> ets:delete(?BEEHIVE_BEE_OBJECT_INFO_TABLE, T)
      end,
      send_to(From, {stopped, RealBeeObject})
  after 20000 ->
      send_to(From, {error, timeout})
  end;
stop(Name, From) ->
  case find_bee(Name) of
    BeeObject when is_record(BeeObject, bee_object)->
      stop(BeeObject#bee_object.pid, From);
    _ ->
      ErrTuple = {error, not_running},
      send_to(From, ErrTuple),
      ErrTuple
  end.

% Unmount the bee
unmount(Type, Name) -> unmount(Type, Name, undefined).
unmount(Type, Name, Caller) ->
  BeforeUnMountScript = case beehive_bee_object_config:get_or_default(unmount, Type) of
    {error, _} = T -> throw(T);
    Str2 -> Str2
  end,
  BeeFile = find_bee_file(Name),
  MountRootDir = config:search_for_application_value(run_dir, ?BEEHIVE_DIR("run")),
  MountDir = filename:join([MountRootDir, Name]),
  UnMountCmd = proplists:get_value(unmount, config_props()),

  BeeObject = from_proplists([{name, Name}, {template, Type}, {bee_file, BeeFile},
                              {run_dir, MountDir}, {bundle_dir, filename:dirname(BeeFile)}
                            ]),

  run_hook_action(pre, BeeObject, Caller),
  Str = template_command_string(UnMountCmd, to_proplist(BeeObject)),
  ?DEBUG_PRINT({run_dir, MountDir, Str}),
  ok = ensure_directory_exists(filename:join([MountDir, "dummy_dir"])),
  case run_in_directory_with_file(BeeObject, Caller, MountDir, BeforeUnMountScript) of
    {error, _Reason} = T1 -> T1;
    T2 ->
      run_command_in_directory(Str, MountDir, Caller, BeeObject),
      run_hook_action(post, BeeObject, Caller),

      send_to(Caller, {unmounted, BeeObject}),
      T2
  end.


cleanup(Name) -> cleanup(Name, undefined).
cleanup(Name, Caller) ->
  case catch find_mounted_bee(Name) of
    {error, _} -> ok;
    MountDir -> ?DEBUG_RM(MountDir)
  end,
  % In this case, beefile is name
  % case catch find_bee_file(Name) of
  %   {error, _} -> ok;
  %   Beefile ->
  %     file:delete(Beefile),
  %     send_to(Caller, {cleaned_up, Name})
  % end.
  send_to(Caller, {cleaned_up, Name}).

% Get information about the Beefile
info(BeeObject) when is_record(BeeObject, bee_object) ->
  case ets:lookup(?BEEHIVE_BEE_OBJECT_INFO_TABLE, BeeObject#bee_object.name) of
    [] ->
      write_info_about_bee(BeeObject),
      to_list(BeeObject);
    ReturnedBeeObjects ->
      case lists:filter(fun({_Name, Dict}) ->
          case dict:is_key(port, Dict) of
            true -> dict:fetch(port, Dict) =:= BeeObject#bee_object.port;
            false -> true
          end
        end, ReturnedBeeObjects) of
      [{_AName, ReturnedDict}|_Rest] -> dict:to_list(ReturnedDict);
      _ ->
        TheReturnedDict = element(2, hd(ReturnedBeeObjects)),
        dict:to_list(TheReturnedDict)
    end
  end;
info(Name) when is_list(Name) ->
  case ets:lookup(?BEEHIVE_BEE_OBJECT_INFO_TABLE, Name) of
    [{Name, Dict}|_Rest] -> dict:to_list(Dict);
    _ ->
      case catch find_bee_file(Name) of
        {error, not_found} -> {error, not_found};
        T -> write_info_about_bee(#bee_object{bee_file = T, name = Name, vcs_type = git})
      end
  end;
info(_Else) ->
  {error, not_found}.

have_bee(Name) ->
  case catch find_bee_file(Name) of
    {error, not_found} -> false;
    _File -> true
  end.

% Send to the node
send_bee_object(ToNode, Name) -> send_bee_object(ToNode, Name, undefined).
send_bee_object(ToNode, Name, Caller) when is_list(Name) ->
  case find_bee(Name) of
    BeeObject when is_record(BeeObject, bee_object) ->
      send_bee_object(ToNode, BeeObject, Caller);
    _ -> {error, not_found}
  end;
send_bee_object(ToNode, #bee_object{bee_file = BeeFile} = BeeObject, Caller) when is_record(BeeObject, bee_object) ->
  case rpc:call(ToNode, code, is_loaded, [?MODULE]) of
    {file, _} -> ok;
    _ ->
      {Mod, Bin, File} = code:get_object_code(?MODULE),
      rpc:call(ToNode, code, load_binary, [Mod, File, Bin])
  end,
  {ok, B} = prim_file:read_file(BeeFile),
  O = rpc:call(ToNode, ?MODULE, save_bee_object, [B, BeeObject]),
  send_to(Caller, {send_bee_object, O}),
  BeeObject.

% Get from a node
get_bee_object(FromNode, Name) ->
  BeeObject = find_bee(Name),
  case rpc:call(FromNode, ?MODULE, send_bee_object, [node(), BeeObject#bee_object{name = Name}]) of
    {badrpc, Err} ->
      io:format("ERROR SAVING ~p to ~p because: ~p~n", [Name, BeeObject, Err]);
    E ->
      E
  end.

% Save
% Save the file contents to
save_bee_object(Contents, #bee_object{bee_file = To} = BeeObject) ->
  FullFilePath = case filelib:is_dir(To) of
    true -> To;
    false ->
      FullPath = filename:join([filename:absname(""), To]),
      ensure_directory_exists(FullPath),
      FullPath
  end,
  prim_file:write_file(FullFilePath, Contents),
  ValidBeeObject = validate_bee_object(BeeObject),
  write_info_about_bee(ValidBeeObject),
  ValidBeeObject.

%%%%%%%%%%%%%%%%%
% HELPERS
%%%%%%%%%%%%%%%%%
run_hook_action(pre, BeeObject, From) -> run_hook_action_str(BeeObject#bee_object.pre, BeeObject, From);
run_hook_action(post, BeeObject, From) -> run_hook_action_str(BeeObject#bee_object.post, BeeObject, From).

run_hook_action_str(CmdStr, #bee_object{bundle_dir = BundleDir} = BeeObject, From) ->
  case CmdStr of
    undefined -> ok;
    _ ->
      case run_in_directory_with_file(BeeObject, From, BundleDir, CmdStr) of
        {error, _} = T -> throw({hook_error, T});
        E -> E
      end
  end.
% Check the url from the type
extract_vcs_type(undefined, Url) when is_list(Url) ->
  case check_type_from_the_url_string(Url, ["git://", "svn://"]) of
    "git://" -> git;
    "svn://" -> svn;
    unknown -> throw({error, unknown_vcs_type})
  end;
extract_vcs_type(VcsType, _Url) when is_atom(VcsType) -> VcsType;
extract_vcs_type(_, _) -> unknown.

% Attempt to extract the type of the vcs from the url
check_type_from_the_url_string(_Str, []) -> unknown;
check_type_from_the_url_string(Str, [H|Rest]) ->
  case string:str(Str, H) of
    0 -> check_type_from_the_url_string(Str, Rest);
    _ -> H
  end.

% Ensure the repos exists with the current revision clone
ensure_repos_exists(#bee_object{bundle_dir = BundleDir} = BeeObject, From) ->
  ensure_directory_exists(BundleDir),
  case filelib:is_dir(BundleDir) of
    true -> update_repos(BeeObject, From);
    false -> clone_repos(BeeObject, From)
  end.

% Checkout the repos using the config method
clone_repos(BeeObject, From)   -> run_action_in_directory(clone, BeeObject, From).
update_repos(BeeObject, From)  -> run_action_in_directory(update, BeeObject, From).

ensure_repos_is_current_repos(#bee_object{revision = Rev} = BeeObject) when is_record(BeeObject, bee_object) ->
  ?DEBUG_PRINT({updating_to_revision, Rev, get_current_sha(BeeObject)}),
  case get_current_sha(BeeObject) of
    {ok, CurrentCheckedRevision} ->
      case CurrentCheckedRevision =:= Rev of
        true -> ok;
        false ->  run_action_in_directory(checkout, BeeObject, undefined)
      end;
    {error, _Lines} = T -> T
  end.

% Get the sha of the bee
get_current_sha(BeeObject) ->
  case run_action_in_directory(check_revision, BeeObject, undefined) of
    {ok, [CurrentCheckedRevision|_Output]} ->
      {ok, chop(CurrentCheckedRevision)};
    T -> {error, T}
  end.


% Run in the directory given in the proplists
% Action
% Props
run_action_in_directory(Action, #bee_object{vcs_type = VcsType, bundle_dir = BundleDir} = BeeObject, From) ->
  case proplists:get_value(Action, config_props(VcsType)) of
    undefined -> throw({error, action_not_defined, Action});
    FoundAction ->
      Str = template_command_string(FoundAction, to_proplist(BeeObject)),
      ?DEBUG_PRINT({run_action_in_directory, action, Action, Str}),
      run_command_in_directory(Str, BundleDir, From, BeeObject)
  end.

% Run a command in the directory
% [] would happen if there is an empty command
run_command_in_directory([], _Dir, _From, _BeeObject) -> ok;
run_command_in_directory(Cmd, Dir, From, BeeObject) ->
  ?DEBUG_PRINT({run_command_in_directory, Dir, Cmd, From}),
  ensure_directory_exists(filename:join([Dir, "does_not_exist"])),
  cmd(Cmd, Dir, to_proplist(BeeObject), From).

% Run file
run_in_directory_with_file(_BeeObject, _From, _Dir, undefined) -> ok;
run_in_directory_with_file(BeeObject, From, Dir, Str) ->
  {ok, Filename, Io} = temp_file(),
  RealStr = case string:str(Str, "#!/bin/") of
    0 -> lists:flatten(["#!/bin/sh -e\n", Str]);
    _ -> Str
  end,
  file:write(Io, RealStr),
  try
    run_command_in_directory(lists:flatten(["/bin/sh ", Filename]), Dir, From, BeeObject)
  after
    file:delete(Filename)
  end.

% Synchronus command
cmd(Str, Cd, Envs, From) ->
  [Exec|Rest] = string:tokens(Str, " "),
  case catch cmd(Exec, Rest, Cd, Envs, From) of
    {'EXIT', T} -> {error, T};
    E -> E
  end.

cmd(Cmd, Args, Cd, Envs, From) ->
  {Pid, Ref, Tag} = async_command(Cmd, Args, Cd, Envs, From),
  receive
    {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
    {'DOWN', Ref, process, Pid, Reason} -> exit(Reason)
  end.

async_command(Cmd, Args, Cd, Envs, From) ->
  Tag = make_ref(),
  {Pid, Ref} = erlang:spawn_monitor(fun() ->
    Rv = cmd_sync(Cmd, Args, Cd, build_envs(Envs), From),
    exit({Tag, Rv})
  end),
  {Pid, Ref, Tag}.

cmd_sync(Cmd, Args, Cd, Envs, From) ->
  StdOpts = [binary, stderr_to_stdout, use_stdio, exit_status, stream, eof, {args, Args}, {env, Envs}],
  Opts = case Cd of
    undefined -> StdOpts;
    _ -> [{cd, Cd}|StdOpts]
  end,
  P = open_port({spawn_executable, os:find_executable(Cmd)},Opts),
  cmd_receive(P, [], From, undefined).

cmd_receive(Port, Acc, From, Fun) ->
  receive
    {Port, {data, Data}}      ->
      List = binary_to_list(Data),
      log_bee_event(List),
      send_to(From, {data, List}),
      run_function(Fun, {data, Data}),
      cmd_receive(Port, [List|Acc], From, Fun);
    {Port, {exit_status, 0}}  ->
      send_to(From, {port_closed, Port}),
      run_function(Fun, {exit_status, 0}),
      {ok, lists:reverse(Acc)};
    {Port, {exit_status, N}}  ->
      send_to(From, {error, N}),
      run_function(Fun, {exit_status, N}),
      port_close(Port),
      {error, {N, lists:reverse(Acc)}};
    E ->
      run_function(Fun, E),
      cmd_receive(Port, Acc, From, Fun)
    after 5000000 ->
      % We don't want it to hang infinitely, so if it does, we'll close it off
      ok
  end.

run_function(undefined, _) -> ok;
run_function(Fun, Msg) when is_function(Fun) -> Fun(Msg).

send_to(undefined, _Msg) -> ok;
send_to(From, Msg) ->
  From ! Msg.

% Ensure the parent directory exists
ensure_directory_exists(Dir) ->
  case filelib:ensure_dir(Dir) of
    {error, enoent} -> ensure_directory_exists(filename:dirname(Dir));
    {error, eexist} -> file:make_dir(Dir);
    O -> O
  end.

% Pull off the config_props for the specific vcs
config_props(VcsType) ->
  case  proplists:get_value(VcsType, config_props()) of
    undefined -> throw({error, unknown_vcs_type});
    Props -> Props
  end.

config_props() ->
  Dir =?BH_ROOT,
  {ok, C} = file:consult(filename:join([Dir, "etc", "beehive_bee_object_config.conf"])),
  C.

% String things
template_command_string(Str, Props) when is_list(Props) -> mustache:render(Str, dict:from_list(Props)).
chop(ListofStrings) -> string:strip(ListofStrings, right, $\n).

from_proplists(Propslist) -> from_proplists(Propslist, #bee_object{}).
from_proplists([], BeeObject) -> validate_bee_object(BeeObject);
from_proplists([{name, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{name = V});
from_proplists([{branch, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{branch = V});
from_proplists([{revision, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{revision = V});
from_proplists([{vcs_type, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{vcs_type = V});
from_proplists([{url, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{url = V});
from_proplists([{template, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{template = V});
from_proplists([{run_dir, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{run_dir = V});
from_proplists([{bundle_dir, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{bundle_dir = V});
from_proplists([{bee_size, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{bee_size = V});
from_proplists([{bee_file, V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{bee_file = V});
from_proplists([{port,V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{port = V});
from_proplists([{pre,V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{pre = V});
from_proplists([{post,V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{post = V});
from_proplists([{os_pid,V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{os_pid = V});
from_proplists([{pid,V}|Rest], BeeObject) -> from_proplists(Rest, BeeObject#bee_object{pid = V});
from_proplists([{Other,V}|Rest], BeeObject) ->
  CurrentEnv = case BeeObject#bee_object.env of
    undefined -> [];
    E -> E
  end,
  from_proplists(Rest, BeeObject#bee_object{env = [{Other,V}|CurrentEnv]}).

to_proplist(BeeObject) -> to_proplist(record_info(fields, bee_object), BeeObject, []).
to_proplist([], _BeeObject, Acc) -> Acc;
to_proplist([name|Rest], #bee_object{name = Name} = Bo, Acc) -> to_proplist(Rest, Bo, [{name, Name}|Acc]);
to_proplist([branch|Rest], #bee_object{branch = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{branch, V}|Acc]);
to_proplist([revision|Rest], #bee_object{revision = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{revision, V}|Acc]);
to_proplist([vcs_type|Rest], #bee_object{vcs_type = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{vcs_type, V}|Acc]);
to_proplist([url|Rest], #bee_object{url = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{url, V}|Acc]);
to_proplist([template|Rest], #bee_object{template = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{template, V}|Acc]);
to_proplist([run_dir|Rest], #bee_object{run_dir = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{run_dir, V}|Acc]);
to_proplist([bundle_dir|Rest], #bee_object{bundle_dir = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{bundle_dir, V}|Acc]);
to_proplist([bee_size|Rest], #bee_object{bee_size = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{bee_size, V}|Acc]);
to_proplist([bee_file|Rest], #bee_object{bee_file = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{bee_file, V}|Acc]);
to_proplist([port|Rest], #bee_object{port = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{port, V}|Acc]);
to_proplist([pre|Rest], #bee_object{pre = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{pre, V}|Acc]);
to_proplist([post|Rest], #bee_object{post = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{post, V}|Acc]);
to_proplist([os_pid|Rest], #bee_object{os_pid = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{os_pid, V}|Acc]);
to_proplist([pid|Rest], #bee_object{pid = V} = Bo, Acc) -> to_proplist(Rest, Bo, [{pid, V}|Acc]);
to_proplist([env|Rest], #bee_object{env = V} = Bo, Acc) -> to_proplist(Rest, Bo, lists:flatten([V|Acc]));
to_proplist([_H|Rest], BeeObject, Acc) -> to_proplist(Rest, BeeObject, Acc).

validate_bee_object(BeeObject) when is_record(BeeObject, bee_object) ->
  validate_bee_object(record_info(fields, bee_object), BeeObject).
validate_bee_object([name|_Rest], #bee_object{name = undefined} = _BeeObject) -> throw({error, no_name_given});
validate_bee_object([bundle_dir|Rest], #bee_object{bundle_dir = undefined, name = Name} = BeeObject) ->
  RootDir = config:search_for_application_value(squashed_dir, ?BEEHIVE_DIR("squashed")),
  RealBundleDir = filename:join([RootDir, Name]),
  validate_bee_object(Rest, BeeObject#bee_object{bundle_dir = RealBundleDir});
validate_bee_object([run_dir|Rest], #bee_object{run_dir = undefined} = BeeObject) ->
  RunDir = config:search_for_application_value(run_dir, ?BEEHIVE_DIR("run")),
  validate_bee_object(Rest, BeeObject#bee_object{run_dir = RunDir});
% Validate branch
validate_bee_object([branch|Rest], #bee_object{branch = undefined} = BeeObject) ->
  validate_bee_object(Rest, BeeObject#bee_object{branch = "master"});
% Validate the bee_file
validate_bee_object([bee_file|Rest], #bee_object{bee_file=undefined} = BeeObject) ->
  RootDir = config:search_for_application_value(squashed_dir, ?BEEHIVE_DIR("squashed")),
  BeeFile = filename:join([RootDir, lists:flatten([unique_filename(BeeObject), ".bee"])]),
  validate_bee_object(Rest, BeeObject#bee_object{bee_file = BeeFile});

% TRy to extract the type
validate_bee_object([vcs_type|Rest], #bee_object{vcs_type=Type, url=Url} = BeeObject) ->
  FoundType = extract_vcs_type(Type, Url),
  validate_bee_object(Rest, BeeObject#bee_object{vcs_type = FoundType});
validate_bee_object([pid|Rest], #bee_object{pid = Pid} = BeeObject) when is_list(Pid) ->
  validate_bee_object(Rest, BeeObject#bee_object{pid = list_to_pid(Pid)});
validate_bee_object([], BeeObject) ->  BeeObject;
validate_bee_object([_H|Rest], BeeObject) -> validate_bee_object(Rest, BeeObject).

unique_filename(#bee_object{name = Name} = _BeeObject) ->
  Name.

% Get temp_file
temp_file() ->
  Filename = test_server:temp_name(atom_to_list(?MODULE)),
  Filepath = filename:join(["/tmp", Filename]),
  {ok, Io} = file:open(Filepath, [write]),
  {ok, Filepath, Io}.

build_envs(Proplists) ->
  lists:flatten(lists:map(fun build_env/1, lists:filter(fun({K, V}) -> valid_env_prop(K, V) end, Proplists))).

build_env({env, V}) -> build_envs(V);
build_env({pid, V}) when is_pid(V) -> {"PID", erlang:pid_to_list(V)};
build_env({K,V}) -> {string:to_upper(to_list(K)), chop(to_list(V))}.

valid_env_prop(pre_action, _V) -> false;
valid_env_prop(before_clone, _V) -> false;
valid_env_prop(after_clone, _V) -> false;
valid_env_prop(post_action, _V) -> false;
valid_env_prop(from, _) -> false;
valid_env_prop(_, undefined) -> false;
valid_env_prop(_, _) -> true.

to_list(undefined) -> "";
to_list(Int) when is_integer(Int) -> erlang:integer_to_list(Int);
to_list(Atom) when is_atom(Atom) -> erlang:atom_to_list(Atom);
to_list(List) when is_list(List) -> List.

find_bee_file(Name) ->
  BundleDir = config:search_for_application_value(squashed_dir, ?BEEHIVE_DIR("squashed")),
  BeeFile = filename:join([BundleDir, lists:flatten([Name, ".bee"])]),
  case filelib:is_file(BeeFile) of
    false -> throw({error, not_found});
    true -> BeeFile
  end.

find_bee(Name) ->
  case info(Name) of
    {error, Reason} -> {error, {not_found, Reason}};
    List -> from_proplists(List)
  end.

find_mounted_bee(Name) ->
  MountRootDir = config:search_for_application_value(run_dir, ?BEEHIVE_DIR("run")),
  MountDir = filename:join([MountRootDir, Name]),
  case filelib:is_dir(MountDir) of
    false -> throw({error, {mounted_bee, not_found, Name}});
    true -> MountDir
  end.

% Cleanup a directory
rm_rf(Dir) ->
  case filelib:is_dir(Dir) of
    true -> bh_file_utils:rm_rf(Dir);
    false -> ok
  end.

read_pid_file_or_retry(_PidFilename, 0) -> {error, no_pidfile};
read_pid_file_or_retry(PidFilename, Retries) ->
  case file:read_file(PidFilename) of
    {ok, Bin} ->
      IntList = chop(erlang:binary_to_list(Bin)),
      case IntList of
        [] -> read_pid_file_or_retry(PidFilename, Retries - 1);
        _Int -> erlang:list_to_integer(IntList)
      end;
    _ ->
      timer:sleep(200),
      read_pid_file_or_retry(PidFilename, Retries - 1)
  end.

% Ets fun
ets_process_restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, start_ets_process, []),
  catch register(?BEEHIVE_BEE_OBJECT_INFO_TABLE_PROCESS, Pid),
  receive
    {'EXIT', Pid, normal} -> % not a crash
      ok;
    {'EXIT', Pid, shutdown} -> % manual termination, not a crash
      ok;
    {'EXIT', Pid, _} ->
      unregister(?BEEHIVE_BEE_OBJECT_INFO_TABLE_PROCESS),
      ets_process_restarter()
  end.

start_ets_process() ->
  TableOpts = [set, named_table, public],
  case catch ets:info(?BEEHIVE_BEE_OBJECT_INFO_TABLE) of
    undefined -> ets:new(?BEEHIVE_BEE_OBJECT_INFO_TABLE, TableOpts);
    _ -> ok
  end,
  receive
    kill -> ok;
    _ -> start_ets_process()
  end.

run_kill_on_pid(_OsPid, BeeDir, RealBeeObject) ->
  % KillStr = lists:flatten(["kill ", integer_to_list(OsPid)]),
  From = self(),
  case beehive_bee_object_config:get_or_default(stop, RealBeeObject#bee_object.template) of
    {error, _} = T ->
      send_to(From, T),
      throw(T);
    StopScript ->
      % Run before, if it needs to run
      run_hook_action(pre, RealBeeObject, From),
      case run_in_directory_with_file(RealBeeObject, From, BeeDir, StopScript) of
        {error, _} = T2 -> send_to(From, T2);
        {ok, Out} ->
          ?DEBUG_PRINT({run_in_directory_with_file, Out}),
          run_hook_action(post, RealBeeObject, From)
      end
  end.

  % ?DEBUG_PRINT({killing_with_string, KillStr}),
  % cmd(KillStr, BeeDir, to_proplist(RealBeeObject), self()),
  % receive
  %   {error, _} = Tuple ->
  %     erlang:display({run_kill_on_pid,error,KillStr,Tuple}),
  %     Tuple;
  %   X ->
  %     erlang:display({run_kill_on_pid,KillStr,X}),
  %     ok
  %   after 5000 ->
  %     ok
  % end.

log_bee_event(Data) ->
  LogFile = filename:join(?BEEHIVE_HOME, "logs/bee_events.log"),
  file:write_file(LogFile, Data, [append]).
