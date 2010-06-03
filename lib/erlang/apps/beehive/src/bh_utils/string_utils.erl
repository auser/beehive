-module (string_utils).

-export([sub/3, gsub/3]).

% Mine
-export ([template_command_string/2]).
-import (string, [len/1, str/2, left/2,right/2,concat/2]).

sub(Str,Old,New) when is_list(Str) andalso is_list(Old) ->
  Lstr = len(Str),
  Lold = len(Old),
  Pos  = str(Str,Old),
  if 
    Pos =:= 0 -> Str;
    true      ->
      LeftPart = left(Str,Pos-1),
      RitePart = right(Str,Lstr-Lold-Pos+1),
      concat(concat(LeftPart,New),RitePart)
  end;
sub(_Str,_Old,New) -> New.

gsub(Str,Old,New) ->
  Acc = sub(Str,Old,New),
  subst(Acc,Old,New,Str).

subst(Str,_Old,_New, Str) -> Str;
subst(Acc, Old, New,_Str) ->
  Acc1 = sub(Acc,Old,New),
  subst(Acc1,Old,New,Acc).

% turn the command string from the comand string with the values
% of [[KEY]] replaced by the corresponding proplist element of
% the format:
%   {[[PORT]], "80"}
template_command_string(OriginalCommand, []) -> OriginalCommand;
template_command_string(OriginalCommand, [{_Str, undefined}|T]) -> template_command_string(OriginalCommand, T);
template_command_string(OriginalCommand, [{Atom, _}|T]) when is_atom(Atom) -> template_command_string(OriginalCommand, T);
template_command_string(OriginalCommand, [{Str, Replace}|T]) ->
  NewCommand = string_utils:gsub(OriginalCommand, Str, Replace),
  template_command_string(NewCommand, T).