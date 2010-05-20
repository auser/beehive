%%%-------------------------------------------------------------------
%%% File    : app_utils.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   This should be defunct soon?
%%% Created :  Fri Jan  8 14:57:14 PST 2010
%%%-------------------------------------------------------------------

-module (app_utils).
-include ("beehive.hrl").
-include ("common.hrl").
-export ([
  app_template_parsed/3
]).

% For testing
-export ([template_proplists/3]).

% Type template
% Get the application type of the template
app_template_parsed(Type, Proplist, DefaultProps) ->
  File = ?USER_OR_BH(["app_templates", "/", Type, ".erl"]),
  {ok, L} = file:consult(File),
  TemplatedStartCommands = template_proplists(L, Proplist, []),
  misc_utils:proplist_merge(TemplatedStartCommands, DefaultProps).
  
% Internal
template_proplists([], _Proplists, Acc) -> lists:reverse(Acc);
template_proplists([{K, V}|Rest], Proplists, Acc) ->
  template_proplists(Rest, Proplists, [{K, template_command_string(V, Proplists)}|Acc]).

template_command_string(List, Proplists) when is_list(List) ->
  case io_lib:char_list(List) of
    true -> string_utils:template_command_string(List, Proplists);
    false -> lists:map(fun(L) -> string_utils:template_command_string(L, Proplists) end, List)
  end;
template_command_string(V, Proplists) ->
  string_utils:template_command_string(V, Proplists).