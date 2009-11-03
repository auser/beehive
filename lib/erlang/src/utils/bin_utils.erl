-module(bin_utils).

-export([starts_with/2, split_lines/1, split_http_lines/1, split_tokens/2,
	 split_first/2, sub_binary/2, sub_binary/3, find_next/2]).


% See if binary has a matching prefix (when predicate needed)
starts_with(Prefix, Binary) when is_binary(Prefix), is_binary(Binary) ->
    Length = size(Prefix),
    case Binary of
	<<Prefix:Length/binary, _More/binary>> ->
	    true;
	_Other ->
	    false
    end.

% Split a binary into lines based on the native newline for the OS
split_lines(Ascii) when is_binary(Ascii) ->
    split_tokens(io_lib:nl(), Ascii).

% Split a binary into lines separated by \r\n as required by HTTP standard
split_http_lines(Ascii) when is_binary(Ascii) ->
    split_tokens(<<"\r\n">>, Ascii).


% Split a binary into binary tokens based on a separator pattern
% Eliminates the pattern from the token results
split_tokens(Int, Binary) when is_integer(Int), is_binary(Binary) ->
    split_tokens(list_to_binary([Int]), Binary);
split_tokens(String, Binary) when is_list(String), is_binary(Binary) ->
    split_tokens(list_to_binary(String), Binary);
split_tokens(Pattern, Binary) when is_binary(Pattern), is_binary(Binary) ->
    split_binary(Binary, Pattern, size(Pattern), 1, 1, [], Binary).


% Get a subsequence of a binary
sub_binary(_Binary, none) ->
    <<>>;
sub_binary(Binary, Start) ->
    sub_binary(Binary, Start, size(Binary)+1).
sub_binary(Binary, Start, End) ->
    Leader = Start - 1,
    case End > size(Binary)+1 of
	true ->
	    Length = size(Binary)+1 - Start;
	false ->
	    Length = End - Start
    end,
    <<_Front:Leader/binary, Data:Length/binary, _Rest/binary>> = Binary,
    Data.


% Split a binary by tracking positional occurrences of Pattern
split_binary(Binary, Pattern, Length, Start, End, Acc, Original) ->

    case Binary of

	% The pattern occurs next in the binary stream
	<<Pattern:Length/binary, Rest/binary>> ->
	    NewStart = End + Length,
	    split_binary(Rest, Pattern, Length, NewStart, NewStart,
			 [sub_binary(Original, Start, End) | Acc], Original);

	% Skip one character and try again
	<<_SkipChar, Rest/binary>> ->
	    split_binary(Rest, Pattern, Length, Start, End + 1, Acc, Original);
			  
	% Done parsing...
	<<>> ->
	    case Start of

		% With no trailing token
		End ->
		    lists:reverse(Acc);

		% With a trailing token
		_Other ->
		    lists:reverse([sub_binary(Original, Start, End) | Acc])
	    end
    end.


% Split on the first occurrence of pattern
split_first(Int, Binary) when is_integer(Int), is_binary(Binary) ->
    split_first(list_to_binary([Int]), Binary);
split_first(String, Binary) when is_list(String), is_binary(Binary) ->
    split_first(list_to_binary(String), Binary);
split_first(Pattern, Binary) when is_binary(Pattern), is_binary(Binary) ->
    Pos = find_next(Binary, Pattern),
    case Pos of
	none ->
	    [Binary];
	Pos ->
	    First = sub_binary(Binary, 1, Pos),
	    Rest = sub_binary(Binary, Pos + size(Pattern)),
	    [First, Rest]
    end.


% Find the next occurrence of pattern
find_next(Binary, Pattern) when is_binary(Binary), is_binary(Pattern) ->
    find_next(Binary, Pattern, size(Pattern), 0).

find_next(<<>>, _Pattern, _Length, _Skipped) ->
    none;
find_next(Binary, Pattern, Length, Skipped) ->
    case Binary of
	<<_Leader:Skipped/binary, Rest/binary>> when size(Rest) < Length ->
	    none;
	<<_Leader:Skipped/binary, Pattern:Length/binary, _Rest/binary>> ->
	    Skipped + 1;
	_Other ->
	    find_next(Binary, Pattern, Length, Skipped + 1)
    end.
