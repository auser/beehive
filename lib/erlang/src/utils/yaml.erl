-module(yaml).
-author("Oliver Mason").
-export([parse/1]).
-export ([parse_file/1]).

% parse a yaml file, represented as a list of lines.
% returns: a data structure as stored in the yaml file.
% version 0.1
% - able to parse basic lists and mappings, but not yet
%   shortcut expressions.

parse_file(Filename) ->
  parse(load_file(Filename)).

parse([]) -> [];
parse(Lines) ->
	Indented = indent_transform(Lines,[]),
	{Tree, []} = parse_tree(0,Indented,[]),
	parse(Tree,[]).


parse_tree(_I, [], Acc) -> {lists:reverse(Acc), []};
parse_tree(Indent, [{Indent, Line}|Rest], Acc) ->
	parse_tree(Indent, Rest, [Line|Acc]);
parse_tree(Indent, [{Ind,Line}|Rest], Acc) when Ind > Indent ->
	{Tree, Remains} = parse_tree(Ind, Rest, [Line]),
	parse_tree(Indent, Remains, [Tree|Acc]);
parse_tree(Indent, [{Ind,Line}|Rest], Acc) when Ind < Indent ->
	{lists:reverse(Acc), [{Ind, Line}|Rest]}.

parse([], Acc) -> lists:reverse(Acc);
parse([Node|Rest], Acc) ->
	case Node of
		[$-] ->	
			[Block|Remains] = Rest,
			parse(Remains,[parse(Block,[])|Acc]);
		[$-,32|Line] ->
			parse(Rest,[Line|Acc]);
		_ ->
			case map_split(Node,[],[]) of
				{Key, Value} -> 
					parse(Rest,[{Key, sanitize(Value)}|Acc]);
				{Key} ->
					[Block|Remains] = Rest,
					parse(Remains,[{Key,parse(Block,[])}|Acc])
			end
	end.

map_split([], Key, []) -> {lists:reverse(Key)};
map_split([], Key, Value) -> {Key, Value};
map_split([$:,32|Value], Key, []) -> {lists:reverse(Key), Value};
map_split([$:|Rest], Key, []) -> map_split(Rest, Key, []);
map_split([Char|Rest], Key, []) -> map_split(Rest, [Char|Key],[]).

sanitize(Str) -> string:strip(Str, both, $\n).

%
% count and remove leading spaces from lines.
% each line is replaced by a tuple of the number of spaces
% and the rest of the line
%
indent_transform([],Acc) ->
	lists:reverse(Acc);
indent_transform([Line|Rest],Acc) ->
	NewLine = indent_transform_line(Line,0),
	indent_transform(Rest,[NewLine|Acc]).

indent_transform_line([32|Rest],Count) ->
	indent_transform_line(Rest,Count+1);
indent_transform_line(Line, Count) ->
	{Count, Line}.



%--------------------------------------------------------
load_file(Filename) ->
	{ok, Infile} = file:open(Filename,[read]),
	Lines = load_file_line(io:get_line(Infile,''), Infile,[]),
	file:close(Infile),
	Lines.

load_file_line(eof, _Infile,Acc) ->
	lists:reverse(Acc);
load_file_line(Line, Infile, Acc) ->
	load_file_line(io:get_line(Infile,''), Infile, [Line|Acc]).
