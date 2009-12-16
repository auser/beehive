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

% Command-line interface
-export ([get_config_option/1]).

-export ([start/0]).

start() ->
  RouterNode = misc_utils:localnode(router),
  
  io:format("RouterNode: ~p~n", [RouterNode]).

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