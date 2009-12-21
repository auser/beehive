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

-record (params, {command, args}).
-define (ALLOWED_TYPE_OF_NODES, [router,node,storage]).
-export ([start/0]).

start() ->
  FullCommand = init:get_plain_arguments(),

  #params{
    command = Command,
    args = Args
    } = parse_args(FullCommand, #params{}),
  
  {NodeType, Node} = get_node_type(),
  
  case (catch command(Node, Command, Args)) of
    {'EXIT', E} ->
      io:format("There was a problem: ~p~n", [E]),
      halt(1);
    unknown_command ->
      io:format("Unknown command: ~p~n", [Command]);
    {error, could_not_connect} ->
      show_could_not_connect_error();
    ok -> ok;
    Else ->
      io:format("Uh oh: ~p~n", [{Else, NodeType}]),
      ok
  end,
  
  halt(0).

command(_Node, app_updated, []) -> error("app_updated requires an argument: name of the app");
command(Node, app_updated, [AppName]) ->
  notify(Node, {app, updated, AppName}),
  ok;
command(_Node, hello, Args) ->
  io:format("Hello world: ~p~n", [Args]),
  hello;
command(_, _Else, _) ->
  unknown_command.

parse_args([H | Args], Params) ->
  Params#params{command = list_to_atom(H), args = Args};
parse_args([], P) -> P.

% Errors
show_could_not_connect_error() ->
  Cookie = erlang:get_cookie(),
  Nodes = nodes(),
  DetectedNodeType = get_node_type(?ALLOWED_TYPE_OF_NODES),
  
  io:format("
    *** Could not connect ***
    There was an error connecting to the local node.
    Your cookie is set as: ~p
    Nodes: ~p
    Detected node type: ~p
", [Cookie, Nodes, DetectedNodeType]),
  halt(2).

error(Msg) ->
  io:format("
    *** ERROR ***
    ~s
    
", [Msg]),
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

notify(Node, Msg) ->
  rpc:call(Node, node_manager, notify, [Msg]).

% INTERNAL
get_node_type() -> get_node_type(?ALLOWED_TYPE_OF_NODES).
get_node_type([]) -> [];
get_node_type([H|Rest]) ->
  case net_adm:ping(misc_utils:localnode(H)) of
    pang -> get_node_type(Rest);
    pong -> {H, misc_utils:localnode(H)}
  end.
  