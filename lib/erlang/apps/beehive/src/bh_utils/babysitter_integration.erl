-module (babysitter_integration).
-include ("beehive.hrl").
-include ("common.hrl").

-export ([command/4]).

command(bundle, App, _Bee, PropLists) ->
  ScratchDisk = proplists:get_value(scratch_dir, PropLists, "/tmp/beehive"),
  SquashedDisk = proplists:get_value(squashed_disk, PropLists, "/tmp/beehive"),
  % WorkingDir  = proplists:get_value(working_dir, PropLists),
  % ReposUrl    = proplists:get_value(repos, PropLists),
  
  WorkingDir      = filename:join([ScratchDisk, App#app.name]),
  SquashedDir     = filename:join([SquashedDisk, App#app.name]),
  FinalLocation   = filename:join([SquashedDir, lists:flatten([App#app.name, ".bee"])]),
  EnvFileLocation = filename:join([SquashedDir, lists:flatten([App#app.name, ".env"])]),
  lists:foreach(fun(Dir) -> file:make_dir(Dir) end, [ScratchDisk, WorkingDir, SquashedDisk, SquashedDir]),

  OtherOpts = [
    {working_directory, WorkingDir},
    {squashed_directory, SquashedDir},
    {env_file, EnvFileLocation},
    {squashed_file, FinalLocation}
    % {repos, ReposUrl}
  ],
  EnvOpts = apps:build_app_env(App, OtherOpts),
  CmdOpts = lists:flatten([{cd, SquashedDir}|EnvOpts]),
  
  babysitter:run(App#app.template, bundle, CmdOpts);

command(mount, App, _Bee, PropLists) ->
  ScratchDisk = proplists:get_value(scratch_dir, PropLists, "/tmp/beehive"),
  RunDir      = proplists:get_value(run_directory, PropLists, "/tmp/beehive"),
  
  UniqueName = App#app.name,

  WorkingDir    = filename:join([ScratchDisk, App#app.name]),
  MountedDir    = filename:join([RunDir, UniqueName]),
  BeeImage      = proplists:get_value(bee_image, PropLists),
  Port          = misc_utils:to_list(proplists:get_value(port, PropLists)),
  
  OtherOpts = [
    {working_directory, WorkingDir},
    {target_directory, MountedDir},
    {bee_image, BeeImage},
    {run_directory, RunDir},
    {port, Port}
  ],
  
  bh_file_utils:ensure_dir_exists([RunDir, ScratchDisk, WorkingDir, MountedDir]),
  
  EnvOpts = apps:build_app_env(App, OtherOpts),
  CmdOpts = lists:flatten([{cd, MountedDir}|EnvOpts]),
  
  babysitter:run(App#app.template, mount, CmdOpts);

% Run the start command
command(start, App, _Bee, PropLists) ->
  Sha = proplists:get_value(sha, PropLists),
  Port = proplists:get_value(port, PropLists),
  ImagePath = proplists:get_value(bee_image, PropLists),
  StorageNode = proplists:get_value(storage_node, PropLists),
    
  HostIp = bh_host:myip(),
  Id = {App#app.name, HostIp, Port},
  StartedAt = date_util:now_to_seconds(),
  
  Bee  = #bee{
    id                      = Id,
    app_name                = App#app.name,
    host                    = HostIp,
    host_node               = node(self()),
    storage_node            = StorageNode,
    % path                    = AppRootPath,
    port                    = Port,
    status                  = pending,
    commit_hash             = Sha,
    start_time              = StartedAt
  },
  
  {ok, _App, CmdOpts1} = case App of
    App when is_record(App, app) -> bees:build_app_env(Bee, App);
    _ -> bees:build_app_env(Bee)
  end,
  CmdOpts = lists:flatten([
    {bee_image, ImagePath},
    CmdOpts1]),
  
  case babysitter:run(App#app.template, start, CmdOpts) of
    {ok, Pid, OsPid} -> {ok, Pid, OsPid, Bee};
    E -> E
  end;

command(stop, CliApp, #bee{os_pid = OsPid} = Bee, _PropLists) ->
  erlang:display({babysitter, kill_pid, OsPid}),
  babysitter:kill_pid(OsPid),
  timer:sleep(200),
  case find_and_build_app_env(CliApp, Bee) of
    {ok, App, CmdOpts} ->
      case babysitter:run(App#app.template, stop, CmdOpts) of
        {ok, _OsPid, _ExitStatus} -> {ok, _OsPid, _ExitStatus, App};
        E -> E
      end;
    Else -> Else
  end;
command(unmount, CliApp, Bee, _PropLists) ->
  case find_and_build_app_env(CliApp, Bee) of
    {ok, App, CmdOpts} ->
      case babysitter:run(App#app.template, unmount, CmdOpts) of
        {ok, _OsPid, _ExitStatus} -> {ok, _OsPid, _ExitStatus, App};
        E -> E
      end;
    Else -> Else
  end;
command(cleanup, CliApp, Bee, _PropLists) ->
  case find_and_build_app_env(CliApp, Bee) of
    {ok, App, CmdOpts} ->
      case babysitter:run(App#app.template, cleanup, CmdOpts) of
        {ok, _OsPid, _ExitStatus} -> {ok, _OsPid, _ExitStatus, App};
        E -> E
      end;
    Else -> Else
  end;
command(Else, _, _, _) ->
  throw({error, unknown_babysitter_command, Else}).

% INTERNAL
find_and_build_app_env(CliApp, Bee) ->
  case CliApp of
    FoundApp when is_record(FoundApp, app) -> bees:build_app_env(Bee, FoundApp);
    unused -> bees:build_app_env(Bee);
    E -> {error, E}
  end.