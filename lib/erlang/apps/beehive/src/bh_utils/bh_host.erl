%%%-------------------------------------------------------------------
%%% File    : bh_host.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Dec 29 18:16:40 PST 2009
%%%-------------------------------------------------------------------

-module (bh_host).

-compile (export_all).

makeip({A1, A2, A3, A4}) ->
  integer_to_list(A1) ++ "." ++
	integer_to_list(A2) ++ "." ++
	integer_to_list(A3) ++ "." ++
	integer_to_list(A4);

makeip({A1, A2, A3, A4, A5, A6, A7, A8}) ->
    A = inet_parse:ntoa({A1, A2, A3, A4, A5, A6, A7, A8}),
    "[" ++ string:to_lower(A) ++ "]".

ip_string_to_tuple(String) ->
  {ok, Tuple} = inet_parse:ipv4_address(String),
  Tuple.

myip() ->
    case get_iplist() of
	[A | _] ->
	    A;
	[] ->
	    "127.0.0.1"
    end.

%% XXX make this return all addresses, currently IPv6 addresses
%% are not returned!
myip_list() ->
    get_iplist().

get_if(L) ->
    get_if(L, []).

get_if([], Res) ->
    Res;

get_if([H | T], Res) ->
    {ok, B} = inet:ifget(H, [addr, flags]),
    {value, {flags, Flags}} = lists:keysearch(flags, 1, B),
    case lists:member(loopback,Flags) of
	true ->
	    %% Ignore interfaces with loopback flag
	    get_if(T, Res);
	false ->
	    case lists:keysearch(addr, 1, B) of
		{value, {addr, Addr}} ->
		    get_if(T, [makeip(Addr) | Res]);
		_ ->
		    %% Interface has no address, might happen on BSD
		    get_if(T, Res)
	    end
    end.

get_iplist() ->
    {ok, If} = inet:getiflist(),
    get_if(If).
    

% Find an unused port
unused_port() -> unused_port_between(5000, 65535, []).
unused_port_between(Start, End, Acc) ->
  ensure_started(crypto),
  Port = crypto:rand_uniform(Start, End),
  case lists:member(Port, Acc) of
    true -> unused_port_between(Start, End, Acc);
    false ->
      case gen_tcp:connect("0.0.0.0", Port, [binary, {active, false}, {packet, raw}]) of
        {ok, _S} -> unused_port_between(Start, End, [Port|Acc]);
        _ -> Port
      end
  end.

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.
