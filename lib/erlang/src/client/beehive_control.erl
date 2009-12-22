%%%-------------------------------------------------------------------
%%% File    : beehive_control.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Eventually, this will allow command-line access to the beehive
%%%   commands
%%%   
%%% Created :  Sun Nov 29 00:13:09 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_control).
-include ("beehive.hrl").

% Command-line interface
-export ([
  get_config_option/1
]).

-record (params, {
  command,
  args,
  node = undefined
}).
-define (ALLOWED_TYPE_OF_NODES, [router,node,storage]).
-export ([start/0]).

start() ->
  FullCommand = init:get_plain_arguments(),

  #params{
    command = Command,
    args = Args,
    node = UserNode
    } = parse_args(FullCommand, #params{}),
  
  Node = case UserNode of
    undefined -> case get_node_type() of
      [] -> show_could_not_connect_error();
      Tuple1 -> Tuple1
    end;
    N -> N
  end,
  
  case (catch command(Node, Command, Args)) of
    {'EXIT', E} ->
      io:format("There was a problem: ~p~n", [E]),
      halt(1);
    unknown_command -> 
      error("Unknown command ~s", [Args]);
    {error, could_not_connect} ->
      show_could_not_connect_error();
    ok -> ok;
    Else ->
      io:format("Uh oh: ~p~n", [{Else}]),
      ok
  end,
  
  halt(0).

% Change the seed on the fly
command(_Node, set_seed, []) -> error("set_seed requires an argument: Seed node");
command(Node, set_seed, [Seed]) ->
  set_seed(Node, Seed),
  ok;
% Notify that the app has been updated
command(_Node, app_updated, []) -> error("app_updated requires an argument: name of the app");
command(Node, app_updated, [AppName]) ->
  notify(Node, {app, updated, AppName}),
  ok;
command(_Node, hello, Args) ->
  io:format("Hello world: ~p~n", [Args]),
  hello;
command(_, _Else, _) ->
  unknown_command.

parse_args([ "-h" | _Rest], _Params) -> show_usage();
parse_args([ "-n", NodeName | Args], Params) ->
  parse_args(Args, Params#params{node = erlang:list_to_atom(NodeName)});
parse_args([H | Args], Params) ->
  Params#params{command = list_to_atom(H), args = Args};
parse_args([], P) -> P.

% Errors
show_could_not_connect_error() ->
  Cookie = erlang:get_cookie(),  
  io:format("
    *** Could not connect ***
    There was an error connecting to the local node.
    Make sure that it is up and that the cookies are identical
    Your cookie is set as: ~p
    
", [Cookie]),
  halt(2).

show_usage() ->
  io:format("
    Usage: beehive_control [OPTIONS] COMMAND
    
    OPTIONS
    -h                              Display this message
    -n                              Local node to connect
    
    COMMANDS
    app_updated [NameOfApp]         Marks an application as updated
    set_seed [SeedNode]             Set a new seed
    
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

% Strictly so we can call out to the config file and pick up options from there
get_config_option([ConfigFile, Param]) ->
  case config:read(ConfigFile) of
    {error, _} -> ok;
    C -> 
      case proplists:get_value(misc_utils:to_atom(Param), C) of
        undefined -> io:format("");
        E -> io:format("~s", [E])
      end
  end.

set_seed(Node, Seed) -> call(Node, node_manager, set_seed, Seed).
notify(Node, Msg) -> call(Node, node_manager, notify, [Msg]).

call(Node, M, F, A) -> 
  case (catch rpc:call(Node, M, F, A)) of
    {'EXIT', _} ->
      error("
There was an error connecting to the node: ~p
Check your configuration and make sure that your node is 
reachable or passing the node name (or not, if you already have)
      ", [Node]),
      show_usage();
    E -> E
  end.
  
% INTERNAL
get_node_type() -> get_node_type(?ALLOWED_TYPE_OF_NODES).
get_node_type([]) -> [];
get_node_type([H|Rest]) ->
  case net_adm:ping(misc_utils:localnode(H)) of
    pang -> get_node_type(Rest);
    pong -> misc_utils:localnode(H)
  end.
  