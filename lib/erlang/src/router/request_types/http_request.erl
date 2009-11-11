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
  Req = request(ClientSock, []),
  RoutingParameter = misc_utils:to_atom(apps:search_for_application_value(routing_parameter, "Host", router)),
  Subdomain = parse_subdomain(proplists:get_value(RoutingParameter, Req#http_request.headers)),
  {ok, Subdomain, Req}.
  
handle_forward(ServerSock, Req) ->
  ReqHeaders = build_request_headers(ServerSock, Req),
  gen_tcp:send(ServerSock, ReqHeaders).
  
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_request_headers(ServerSock,
    #http_request{length = Length, headers = Headers, path = Path, method = Method, version = Version} = Req
  ) ->  
  {ok, Hostname} = inet:gethostname(),
  
  NewHeaders = replace_values_in_prolists(
      [
        {'Content-Length', Length},
        {'X-Forwarded-For', Hostname}
      ],
      Headers),

  End = headers_to_list(NewHeaders),
  
  case body_length(Req) of
    chunked -> 
      Body = receive_body(ServerSock, []),
      iolist_to_binary([misc_utils:to_list(Method), " ", Path, " HTTP/", version(Version), <<"\r\n">>, End, Body]);
    _ -> 
      [misc_utils:to_list(Method), " ", Path, " HTTP/", version(Version), <<"\r\n">> | End]
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

request(Socket, Body) ->
  case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
    {ok, {http_request, Method, {abs_path, Path}, Version}} ->
      Req = #http_request{method = Method, path = Path, version = Version, client_socket = Socket},
      headers(Socket, Req, 0);
    {error, {http_error, "\r\n"}} ->
      request(Socket, Body);
    {error, {http_error, "\n"}} ->
      request(Socket, Body);
    Other ->
      ?LOG(info, "request received something else: ~p", [Other]),
      gen_tcp:close(Socket),
      exit(normal)
  end.

headers(Socket, _Request, Headers) when Headers > ?MAX_HEADERS ->
  %% Too many headers sent, bad request.
  inet:setopts(Socket, [{packet, raw}]),
  gen_tcp:send(Socket, ?ERROR_HTML("Uh oh... too many headers")),
  gen_tcp:close(Socket),
  exit(normal);

headers(Socket, #http_request{headers = Headers} = Request, HeaderCount) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, http_eoh} ->
      inet:setopts(Socket, [binary, {packet, 0}, {active, false}]),
      Request;
    {ok, {http_header, _, Name, _, Value}} ->
      headers(Socket, Request#http_request{headers = [{Name, Value} | Headers]}, 1 + HeaderCount);
    Other ->
      ?LOG(info, "headers received something else: ~p", [Other]),
      gen_tcp:close(Socket),
      exit(normal)
  end.

body_length(#http_request{headers = Headers} = Request) ->
  case proplists:get_value('Transfer-Encoding', Headers) of
    undefined ->
        case proplists:get_value('Content-Length', Headers) of
          undefined -> undefined;
          L -> erlang:list_to_integer(L)
        end;
    Value ->
      case Value of
        "Chunked" -> chunked;
        "chunked" -> chunked;
        Else -> {error, Else}
      end
  end.
  
receive_body(Socket, BodyAcc) ->
  case gen_tcp:recv(Socket, 1024, 3000) of
      {ok, Data} ->
        receive_body(Socket, [Data|BodyAcc]);
      Else ->
        io:format("Else: ~p~n", [Else]),
        iolist_to_binary(lists:reverse(BodyAcc))
  end.