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
    {start, ClientSock, RequestPid} ->
      {ok, RoutingKey, Req} = http_request:handle_request(ClientSock),
      engage_backend(ClientSock, RequestPid, RoutingKey, Req, app_srv:get_backend(self(), RoutingKey));
    _E ->
      proxy_init(ClientSock)
  after ?IDLE_TIMEOUT ->
    % We only give 30 seconds to the proxy pid to be given control of the socket
    % this is MORE than enough time for the socket to be given a chance to prepare
    % to be handled by the proxy accept request.
    ?LOG(error, "Proxy is b0rked because of timeout", []),
    exit(error)
  end.

engage_backend(ClientSock, RequestPid, Hostname, Req, {ok, #backend{host = Host, port = Port} = Backend}) ->
  SockOpts = [  binary, 
                {nodelay, true},
				        {active, once},
				        {packet, 0},
				        {reuseaddr, true}
			       ],
  case gen_tcp:connect(Host, Port, SockOpts) of
    {ok, ServerSock} ->
      ?NOTIFY({backend, used, Backend}),
      http_request:handle_forward(ServerSock, ClientSock, Req, self()),      
      inet:setopts(ServerSock, [{active, once}]),
      proxy_loop(#state{
                  start_time = date_util:now_to_seconds(),
                  client_socket = ClientSock, 
                  server_socket = ServerSock, 
                  request = Req,
                  subdomain = Hostname,
                  backend = Backend}
                );
    Error ->
      ?LOG(error, "Connection to remote TCP server: ~p:~p ~p", [Host, Port, Error]),
      ?NOTIFY({backend, cannot_connect, Backend}),
      timer:sleep(200),
      engage_backend(ClientSock, RequestPid, Hostname, Req, app_srv:get_backend(RequestPid, Hostname))
  end;
engage_backend(ClientSock, _RequestPid, Hostname, _Req, ?BACKEND_TIMEOUT_MSG) ->
  ?LOG(error, "Error getting backend because of timeout: ~p", [Hostname]),
  send(ClientSock, ?APP_ERROR("Something went wrong: Timeout on backend")),
  gen_tcp:close(ClientSock),
  exit(error);
engage_backend(ClientSock, _RequestPid, Hostname, _Req, {error, Reason}) ->
  ?LOG(error, "Backend for ~p was not found: ~p", [Hostname, Reason]),
  send(ClientSock, ?APP_ERROR(io_lib:format("Error: ~p", [Reason]))),
  gen_tcp:close(ClientSock),
  exit(error);
engage_backend(ClientSock, _RequestPid, Hostname, _Req, Else) ->
  ?LOG(info, "proxy_handler received other message for ~p: ~p", [Hostname, Else]),
  gen_tcp:close(ClientSock),
  exit(error).

% Handle all the proxy here
proxy_loop(#state{client_socket = CSock, server_socket = SSock} = State) ->
  receive
	  {tcp, CSock, Data} ->
      % Received data from the client
	    gen_tcp:send(SSock, Data),
	    inet:setopts(CSock, [{active, once}]),
	    proxy_loop(State);
  	{tcp, SSock, Data} ->
      % Received info from the server
      % case re:run(Data, "HTTP/1.1 100 [Cc]ontinue", []) of
      %   nomatch ->
      %     send(CSock, Data),
      %     inet:setopts(SSock, [{active, once}]);
      %   {match, _} ->
      %     inet:setopts(SSock, [{active, once}])
      % end,
      % case re:run(Data, "[Cc]onnection: [Cc]lose", []) of
      %   nomatch ->
      %     inet:setopts(SSock, [{active, once}]),
      %     proxy_loop(State);
      %   {match, _} ->
      %     terminate(normal, State)
      % end;
      send(CSock, Data),
      inet:setopts(SSock, [{active, once}]),
      proxy_loop(State);
      % case re:run(Data, "100 [Cc]ontinue", []) of
      %   {match, _} ->
      %     proxy_loop(State);
      %   nomatch -> 
      %     inet:setopts(SSock, [{active, false}]),
      %     case gen_tcp:recv(SSock, 0, 500) of
      %       {ok, D} ->
      %         send(CSock, D),
      %         inet:setopts(SSock, [{active, once}]),
      %         proxy_loop(State);
      %       {error, timeout} -> terminate(normal, State);
      %       {error, closed} -> terminate(normal, State);
      %       {error, E} -> 
      %         ?LOG(info, "Error with SSock: ~p",[E]),
      %         terminate(E, State)
      %     end
      % end;
  	{tcp_closed, CSock} ->
      % ?LOG(info, "Client closed connection", []),
  	  terminate(normal, State);
		{tcp_closed, SSock} ->
      % ?LOG(info, "Server closed connection", []),
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
  after 120000 ->
    ?LOG(info, "Terminating open proxy connection because of timeout", []),
    terminate(normal, State)
  end.

% Close the connection.
terminate(Reason, #state{server_socket = SSock, client_socket = CSock, start_time = STime, backend = Backend} = _State) ->
  StatsProplist1 = [{elapsed_time, date_util:now_to_seconds() - STime}],
  StatsProplist = case inet:getstat(CSock, [recv_cnt]) of
    {ok, D} -> [{socket, D}|StatsProplist1];
    _ -> StatsProplist1
  end,

  ?NOTIFY({backend, closing_stats, Backend, StatsProplist}),
  gen_tcp:close(SSock), gen_tcp:close(CSock),
  exit(Reason).

% Send data across a socket
send(Sock, Data) ->
  gen_tcp:send(Sock, Data).
