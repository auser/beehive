-module (babysitter_integration).
-include ("beehive.hrl").
-include ("common.hrl").

-export ([command/4]).

command(bundle, App, _Bee, PropLists) ->
  ScratchDisk = proplists:get_value(scratch_disk, PropLists, "/tmp/beehive"),
  SquashedDisk = proplists:get_value(squashed_disk, PropLists, "/tmp/beehive"),
  % WorkingDir  = proplists:get_value(working_dir, PropLists),
  % ReposUrl    = proplists:get_value(repos, PropLists),
  
  WorkingDir = lists:flatten([ScratchDisk, "/", App#app.name]),
  SquashedDir = lists:flatten([SquashedDisk, "/", App#app.name]),
  FinalLocation = lists:flatten([SquashedDir, "/", App#app.name, ".bee"]),
  EnvFileLocation = lists:flatten([SquashedDir, "/", App#app.name, ".env"]),
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
  ScratchDisk = config:search_for_application_value(scratch_disk, ?BEEHIVE_DIR("tmp")),
  RunDir = config:search_for_application_value(squashed_storage, ?BEEHIVE_DIR("run")),
  UniqueName = App#app.name,

  WorkingDir = filename:join([ScratchDisk, App#app.name]),
  MountedDir = filename:join([RunDir, UniqueName]),
  BeeImage = proplists:get_value(bee_image, PropLists),
  Port = misc_utils:to_list(proplists:get_value(port, PropLists)),
  
  OtherOpts = [
    {working_directory, WorkingDir},
    {target_directory, MountedDir},
    {bee_image, BeeImage},
    {run_dir, RunDir},
    {port, Port}
  ],
  lists:map(fun(Dir) -> file:make_dir(Dir) end, [RunDir, ScratchDisk, WorkingDir, MountedDir]),
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
      
  io:format("------ App handler using babysitter spawn_new: ~p~n", [CmdOpts]),
  case babysitter:run(App#app.template, start, CmdOpts) of
    {ok, Pid, OsPid} -> {ok, Pid, OsPid, Bee};
    E -> E
  end;

command(stop, CliApp, Bee, _PropLists) ->
  {ok, App, CmdOpts} = case CliApp of
    FoundApp when is_record(FoundApp, app) -> bees:build_app_env(Bee, FoundApp);
    {error, _} = T -> T;
    _ -> bees:build_app_env(Bee)
  end,
  case babysitter:run(App#app.template, stop, CmdOpts) of
    {ok, _OsPid, _ExitStatus} -> {ok, _OsPid, _ExitStatus, App};
    E -> E
  end;
command(unmount, CliApp, Bee, _PropLists) ->
  {ok, App, CmdOpts} = case CliApp of
    FoundApp when is_record(FoundApp, app) -> bees:build_app_env(Bee, FoundApp);
    {error, _} = T -> T;
    _ -> bees:build_app_env(Bee)
  end,
  case babysitter:run(App#app.template, unmount, CmdOpts) of
    {ok, _OsPid, _ExitStatus} -> {ok, _OsPid, _ExitStatus, App};
    E -> E
  end;
command(cleanup, CliApp, Bee, _PropLists) ->
  {ok, App, CmdOpts} = case CliApp of
    FoundApp when is_record(FoundApp, app) -> bees:build_app_env(Bee, FoundApp);
    {error, _} = T -> T;
    _ -> bees:build_app_env(Bee)
  end,
  case babysitter:run(App#app.template, cleanup, CmdOpts) of
    {ok, _OsPid, _ExitStatus} -> {ok, _OsPid, _ExitStatus, App};
    E -> E
  end;
command(Else, _, _, _) ->
  throw({error, unknown_babysitter_command, Else}).