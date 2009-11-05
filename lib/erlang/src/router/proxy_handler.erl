%%%-------------------------------------------------------------------
%%% File    : proxy_handler.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Oct 13 20:24:21 PDT 2009
%%%-------------------------------------------------------------------

-module (proxy_handler).

-include ("common.hrl").
-include ("http.hrl").
-include ("router.hrl").

-export ([
  start_link/1
]).

% Internal API
% -export ([terminate/2]).

-record(state, {
  subdomain,        % subdomain of the app to look for
  backend,          % Backend
  request,          % client request
  start_time,       % time proxy started
  client_socket,    % client socket
  server_socket,    % server socket
  timeout           % timeout
}).

start_link(ClientSock) ->
  Pid = spawn_link(fun() -> proxy_init(ClientSock) end),
  {ok, Pid}.

proxy_init(ClientSock) ->
  receive
    {start, ClientSock, BalancerPid} ->
      link(BalancerPid),
      % Chose here the type of response... for now, it'll just be http, but in the future... maybe tcp?
      inet:setopts(ClientSock, [{packet, http}]),
      Req = request(ClientSock, []),
      Subdomain = parse_subdomain(proplists:get_value('Host', Req#http_request.headers)),
      engage_backend(ClientSock, BalancerPid, Subdomain, Req, app_srv:get_backend(self(), Subdomain));
    _E ->
      proxy_init(ClientSock)
  after 10000 ->
    % We only give 10 seconds to the proxy pid to be given control of the socket
    % this is MORE than enough time for the socket to be given a chance to prepare
    % to be handled by the proxy accept request. It should be milliseconds
    ?LOG(error, "Proxy is b0rked because of timeout", []),
    exit(error)
  end.

engage_backend(ClientSock, BalancerPid, Hostname, Req, {ok, #backend{host = Host, port = Port} = Backend}) ->
  % This might be a little heavyweight... to connect to the backend on every single request.
  % TODO: Add connected sockets in the background to return only the backends that are accessible
  SockOpts = [binary, {nodelay, true},
				              {active, true}, 
				              {recbuf, ?BUFSIZ},
			                {sndbuf, ?BUFSIZ},
			                {send_timeout, 5000}],
  case gen_tcp:connect(Host, Port, SockOpts) of
    {ok, ServerSock} ->
      app_srv:remote_ok(Backend, self()),
      send_initial_request(ServerSock, Req),
      proxy_loop(#state{client_socket = ClientSock, server_socket = ServerSock, request = Req, backend = Backend});
    Error ->
      ?LOG(error, "Connection to remote TCP server: ~p:~p ~p", [Host, Port, Error]),
      app_srv:remote_error(Backend, Error),
      engage_backend(ClientSock, BalancerPid, Hostname, Req, app_srv:get_backend(self(), Hostname))
  end;
engage_backend(ClientSock, _BalancerPid, Hostname, _Req, ?BACKEND_TIMEOUT_MSG) ->
  ?LOG(error, "Error getting backend because of timeout: ~p", [Hostname]),
  send(ClientSock, ?APP_ERROR("Something went wrong: Timeout on backend")),
  gen_tcp:close(ClientSock),
  exit(error);
engage_backend(ClientSock, _BalancerPid, Hostname, _Req, {error, Reason}) ->
  ?LOG(error, "Backend for ~p was not found: ~p", [Hostname, Reason]),
  send(ClientSock, ?APP_ERROR(io_lib:format("Error: ~p", [Reason]))),
  gen_tcp:close(ClientSock),
  exit(error);
engage_backend(ClientSock, _BalancerPid, Hostname, _Req, Else) ->
  ?LOG(info, "proxy_handler received other message for ~p: ~p", [Hostname, Else]),
  gen_tcp:close(ClientSock),
  exit(error).

% Handle all the proxy here
proxy_loop(#state{client_socket = CSock, server_socket = SSock} = State) ->
  receive
	  {tcp, CSock, Data} ->
      % Received data from the client
      % ?LOG(info, "Received info from the client: ~p", [Data]),
	    gen_tcp:send(SSock, Data),
	    inet:setopts(CSock, [{active, once}]),
	    proxy_loop(State);
  	{tcp, SSock, Data} ->
      % Received info from the server
      % ?LOG(info, "Received info from the server: ~p", [Data]),
      send(CSock, Data),
      inet:setopts(SSock, [{active, false}, {packet, raw}]),
      case gen_tcp:recv(SSock, 0, 500) of
        {ok, D} ->
          send(CSock, D),
          inet:setopts(SSock, [{active, once}]),
          proxy_loop(State);
        {error, _E} -> terminate(normal, State)
      end;
  	{tcp_closed, CSock} ->
      % ?LOG(info, "Client closed connection", []),
  	  gen_tcp:close(SSock),
  	  terminate(normal, State);
		{tcp_closed, SSock} ->
      % ?LOG(info, "Server closed connection", []),
  	  gen_tcp:close(CSock),
  	  terminate(normal, State);
  	{tcp_error, SSock} ->
  	  ?LOG(error, "tcp_error on server: ~p", [SSock]),
      gen_tcp:close(CSock),
      terminate(normal, State);
    ?BACKEND_TIMEOUT_MSG ->
      % ?LOG(info, "Backend timeout message", []),
      terminate(timeout, State);
  	Msg ->
	    ?LOG(info, "~s:proxy_loop: unexpectedly recieved ~w\n", [?MODULE, Msg]),
	    proxy_loop(State)
  after 60000 ->
    ?LOG(info, "Terminating open proxy connection because of timeout", []),
    terminate(normal, State)
  end.

% Close the connection.
terminate(Reason, #state{server_socket = SSock, client_socket = CSock} = _State) ->
  gen_tcp:close(SSock), gen_tcp:close(CSock),
  exit(Reason).

send_initial_request(ServerSock, Req) ->
  ReqHeaders = build_request_headers(Req),
  send(ServerSock, ReqHeaders).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_request_headers(#http_request{headers = Headers, path = Path, method = Method, version = Version} = _Req) ->  
  Length = length([]), % maybe?!?!
  {ok, Hostname} = inet:gethostname(),
  
  NewHeaders = replace_values_in_prolists(
      [
        {'Content-Length', Length},
        {'X-Forwarded-For', Hostname}
      ],
      Headers),

  End = headers_to_list(NewHeaders),

  [misc_utils:to_list(Method), " ", Path, " HTTP/", version(Version), <<"\r\n">> | End].

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

% Send data across a socket
send(Sock, Data) ->
  gen_tcp:send(Sock, Data).
  
% HTTP
parse_subdomain(HostName) ->
  StrippedHostname = lists:takewhile(fun (C) -> C =/= $: end, HostName),
  lists:takewhile(fun (C) -> C =/= $. end, StrippedHostname).

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

headers(Socket, _Request, ?MAX_HEADERS) ->
  %% Too many headers sent, bad request.
  inet:setopts(Socket, [{packet, raw}]),
  send(Socket, ?ERROR_HTML("Uh oh... too many headers")),
  gen_tcp:close(Socket),
  exit(normal);

headers(Socket, #http_request{headers = Headers} = Request, HeaderCount) ->
  case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
    {ok, http_eoh} ->
      inet:setopts(Socket, [{packet, raw}]),
      Request;
    {ok, {http_header, _, Name, _, Value}} ->
      headers(Socket, Request#http_request{headers = [{Name, Value} | Headers]}, 1 + HeaderCount);
    Other ->
      ?LOG(info, "headers received something else: ~p", [Other]),
      gen_tcp:close(Socket),
      exit(normal)
  end.
