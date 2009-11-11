%%%-------------------------------------------------------------------
%%% File    : http_request.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 10:46:44 PST 2009
%%%-------------------------------------------------------------------

-module (http_request).

-include ("router.hrl").
-include ("http.hrl").
-include ("common.hrl").

%% API
-export([
  handle_request/1,
  handle_forward/2
]).

% Take the connecting socket and handle the request. Get the request up until the end of the headers
% Take off the 'Host' parameter (or other sepcified parameter) from the header and return the 
% routing key and the full request to the calling process
handle_request(ClientSock) ->
  inet:setopts(ClientSock, [{packet, http}]),
  Req = request(ClientSock, 
    fun(MochiReq) ->
      MochiReq
    end
  ),
  
  RoutingParameter = misc_utils:to_atom(apps:search_for_application_value(routing_parameter, "Host", router)),
  
  HeaderVal = mochiweb_headers:get_value(RoutingParameter, Req:get(headers)),
  Subdomain = parse_subdomain(HeaderVal),
  {ok, Subdomain, Req}.
  
handle_forward(ServerSock, Req) ->
  ReqHeaders = build_request_headers(ServerSock, Req),
  gen_tcp:send(ServerSock, ReqHeaders).
  
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_request_headers(ServerSock, Req) ->  
  {ok, Hostname} = inet:gethostname(),
    
  Headers = mochiweb_headers:to_list(Req:get(headers)),
  Method = Req:get(method),
  Path = Req:get(raw_path),
  Version = Req:get(version),
  
  NewHeaders = replace_values_in_prolists(
      [
        {'X-Forwarded-For', Hostname}
      ],
      Headers),

  case Req:recv_body() of
    undefined -> 
      [misc_utils:to_list(Method), " ", Path, " HTTP/", version(Version), <<"\r\n">> | headers_to_list(NewHeaders)];
    BElse ->
      ([
        misc_utils:to_list(Method), " ", Path, " HTTP/", version(Version), <<"\r\n">>,
        headers_to_list(NewHeaders),
        BElse
      ])
  end.

% Replace values in a proplist
replace_values_in_prolists(PropList, OriginalHeaders) ->
  lists:flatten(lists:map(fun({K,V}) ->
      OldHeaders = case proplists:is_defined(K, OriginalHeaders) of
        true -> proplists:delete(K, OriginalHeaders);
        false -> OriginalHeaders
      end,
      [{K, V}|OldHeaders]
    end, PropList)).

% Turn a tuple string of the version into a string
version({1,1}) -> "1.1";
version(_) -> "1.0".

% Turn a proplist of headers into a string
headers_to_list(Headers) ->
  F = fun ({K, V}, Acc) -> [misc_utils:to_list(K), <<": ">>, misc_utils:to_list(V), <<"\r\n">> | Acc] end,
  lists:foldl(F, [<<"\r\n">>], Headers).

% HTTP
parse_subdomain(HostName) ->
  NumberOfPeriods = length([X || X <- HostName, X =:= $.]),
  if
    NumberOfPeriods > 1 ->
      StrippedHostname = lists:takewhile(fun (C) -> C =/= $: end, HostName),
      lists:takewhile(fun (C) -> C =/= $. end, StrippedHostname);
    true -> base
  end.

% FROM MOCHIWEB
request(Socket, Body) ->
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, {http_request, Method, Path, Version}} ->
            headers(Socket, {Method, Path, Version}, [], Body, 0);
        {error, {http_error, "\r\n"}} ->
            request(Socket, Body);
        {error, {http_error, "\n"}} ->
            request(Socket, Body);
        _Other ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

headers(Socket, Request, Headers, _Body, ?MAX_HEADERS) ->
  %% Too many headers sent, bad request.
  inet:setopts(Socket, [{packet, raw}]),
  Req = mochiweb:new_request({Socket, Request,lists:reverse(Headers)}),
  Req:respond({400, [], []}),
  gen_tcp:close(Socket),
  exit(normal);
headers(Socket, Request, Headers, Body, HeaderCount) ->
  case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
    {ok, http_eoh} ->
      inet:setopts(Socket, [{packet, raw}]),
      Req = mochiweb:new_request({Socket, Request, lists:reverse(Headers)}),
      Body(Req);
    {ok, {http_header, _, Name, _, Value}} ->
      headers(Socket, Request, [{Name, Value} | Headers], Body, 1 + HeaderCount);
    _Other ->
      gen_tcp:close(Socket),
      exit(normal)
  end.