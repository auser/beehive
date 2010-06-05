% Beehive request
-module (beehive_request).
-include ("router.hrl").
-export ([
  new/1
]).


% Create a new beehive request object from a socket
new(ClientSock) ->
  inet:setopts(ClientSock, [{packet, http}]),
  request(ClientSock).

% TODO: Change this...
request(Socket) ->
  case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
    {ok, {http_request, Method, Path, Version}} ->
      headers(Socket, {Method, Path, Version}, [], 0);
    {error, {http_error, "\r\n"}} ->
      request(Socket);
    {error, {http_error, "\n"}} ->
      request(Socket);
    _Other ->
      gen_tcp:close(Socket),
      exit(normal)
  end.

headers(Socket, Request, Headers, ?MAX_HEADERS) ->
  %% Too many headers sent, bad request.
  inet:setopts(Socket, [{packet, raw}]),
  Req = mochiweb:new_request({Socket, Request,lists:reverse(Headers)}),
  Req:respond({400, [], []}),
  gen_tcp:close(Socket),
  exit(normal);
headers(Socket, Request, Headers, HeaderCount) ->
  case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
    {ok, http_eoh} ->
      inet:setopts(Socket, [{packet, raw}]),
      mochiweb:new_request({Socket, Request, lists:reverse(Headers)});
    {ok, {http_header, _, Name, _, Value}} ->
      headers(Socket, Request, [{Name, Value} | Headers], 1 + HeaderCount);
    _Other ->
      gen_tcp:close(Socket),
      exit(normal)
  end.

