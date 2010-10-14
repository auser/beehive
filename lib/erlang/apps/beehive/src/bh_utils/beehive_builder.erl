-module (beehive_builder).
-include ("beehive.hrl").
-include ("common.hrl").

-compile({no_auto_import,[error/1, error/2]}).

-record (params, {
  command   = undefined,
  app_file,
  root_dir  = undefined,
  port      = 5001,
  args      = []
}).

-export ([start/0]).

start() ->
  FullCommand = init:get_plain_arguments(),

  #params{
    app_file = AppFile,
    command = Command,
    root_dir = RootDir1,
    port      = Port,
    args = CliArgs1
    } = parse_args(FullCommand, #params{}),
  
  App = build_app_from_args(AppFile),
  
  RootDir = case RootDir1 of
    undefined -> 
      {ok, D} = file:get_cwd(),
      D;
    _ -> RootDir1
  end,

  application:set_env(beehive, beehive_home, RootDir),
  
  % Build all the args here
  ScratchDisk = config:search_for_application_value(scratch_dir, "/tmp/beehive/scratch"),  
  SquashedDisk = config:search_for_application_value(squashed_disk, "/tmp/beehive/squashed"),
  BeeImage = filename:join([filename:absname(SquashedDisk), App#app.name, lists:append([App#app.name, ".bee"])]),
  HostIp = bh_host:myip(),
  
  CliArgs = [
    {bee_image, BeeImage},
    {port, Port},
    {host, HostIp},
    {scratch_dir, ScratchDisk}, 
    {squashed_disk, SquashedDisk}
  |CliArgs1],
  
  ok = validate_args(CliArgs),
  Bee = build_bee_from_args(App, CliArgs),
  
  start_it_up(),
  case babysitter_integration:command(Command, App, Bee, CliArgs) of
    {ok, OsPid, ExitCode} ->
io:format("
  Ran the babysitter command: ~p
    Ran with the OSPid of: ~p
    Exit code: ~p
  ", [Command, OsPid, ExitCode]);
    {ok, ErlPid, OsPid, Model} ->
io:format("
  Successfully executed the command: ~p
    Ran with the OSPid of: ~p and erlang pid: ~p
    Bee id: ~p
  ", [Command, OsPid, ErlPid, Model]);
    {error, Phase, OsPid, ExitCode, StdOut, StdErr} ->
io:format("
  Command ~p failed at the stage: ~p
    Ran with the OSPid of: ~p
    Exit code: ~p
    Stdout: ~s
    Stderr: ~s
  ", [Command, Phase, OsPid, ExitCode, StdOut, StdErr]);
  {error, no_command} ->
io:format("
  No command is described in the application type for this app~n
", [])
  end,
  shut_it_down().
  
%% INTERNAL

start_it_up() ->
  % application:start(sasl),
  application:start(babysitter),
  node_manager:read_babysitter_config().

shut_it_down() ->
  application:stop(babysitter).
  
parse_args([ "-h" | _Rest], _Params) -> show_usage();
parse_args([ "-a", AppFile | Rest], Params) -> parse_args(Rest, Params#params{app_file = AppFile});
parse_args([ "-d", Dir | Rest], Params) -> parse_args(Rest, Params#params{root_dir = Dir});
parse_args([ "-p", Port | Rest], Params) -> parse_args(Rest, Params#params{port = list_to_integer(Port)});
parse_args([H | Rest], #params{command = undefined} = Params) -> 
  parse_args(Rest, Params#params{command = list_to_atom(H)});
parse_args([H | Rest], #params{args = CurrArgs} = Params) -> 
  parse_args(Rest, Params#params{args = lists:flatten([H|CurrArgs])});
parse_args([], P) -> P.


build_app_from_args(undefined) -> [];
build_app_from_args(Args) -> 
  Proplist = fetch_file_proplist(Args),
  apps:new(Proplist).
  
build_bee_from_args(undefined, _) -> [];
build_bee_from_args(#app{name = AppName, revision = Sha} = _App, Args) -> 
  Port = proplists:get_value(port, Args),
  Host = proplists:get_value(host, Args),
  StartedAt = date_util:now_to_seconds(),
  
  #bee{
    id                      = {AppName, Host, Port},
    app_name                = AppName,
    host                    = Host,
    host_node               = node(self()),
    port                    = Port,
    status                  = pending,
    revision             = Sha,
    start_time              = StartedAt
  }.

fetch_file_proplist(Filename) ->
  case filelib:is_file(Filename) of
    true -> 
      {ok, Props} = file:consult(Filename),
      Props;
    false ->
      {ok, Dir} = file:get_cwd(),
      NewFilename = filename:join(Dir, Filename),
      case filelib:is_file(NewFilename) of
        true -> 
          {ok, Props} = file:consult(Filename),
          Props;
        false -> []
      end
  end.

show_usage() ->
  io:format("
    Usage: beehive_control [OPTIONS] COMMAND

    OPTIONS
    -h                              Display this message
    -a [file]                       File to load app descriptors
    -b [file]                       File to load bee descriptors
    -l                              Local node to connect

    COMMANDS
    stop                            Stop the beehive server entirely
    app_updated [NameOfApp]         Marks an application as updated
    set_seed [SeedNode]             Set a new seed
    get_seed                        Get the seed node
    reload                          Reload the beehive system
    list [Type (optional)]          List types (valid types: router, storage, node or nothing)

", []),
halt(1).

error(Msg) ->
  error("~s", [Msg]).
error(Format, Args) ->
  Str = io_lib:format(Format, Args),
  io:format("
    *** ERROR ***
    ~s
", [Str]),
  show_usage(),
  halt(2).

validate_args([]) -> ok;
validate_args([ {port, Port} | Rest]) ->
  case Port =< 65535 andalso Port >= 5000 of
    true -> validate_args(Rest);
    false -> 
      error("Invalid port. The port must be between 5000 and 65535")
  end;
validate_args([_K|Rest]) -> validate_args(Rest).
