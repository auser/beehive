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

-record(state, {
  subdomain,        % subdomain of the app to look for
  bee,              % Backend
  request,          % client request
  start_time,       % time proxy started
  client_socket,    % client socket
  server_socket,    % server socket
  timeout           % timeout
}).

% Start the proxy by spawning off an init with the socket.
% Compatible with the supervisor behaviour
start_link(ClientSock) ->
  Pid = spawn_link(fun() -> proxy_init(ClientSock) end),
  {ok, Pid}.

% Receive the initial request and socket. Use the packet_decoder (http for now) to decode the
% routing key required. If this can be found, then start engaging the bee.
% If this takes more than IDLE_TIMEOUT seconds, then exit with an error.
proxy_init(ClientSock) ->
  receive
    {start, ClientSock, RequestPid} ->
      {ok, RoutingKey, ForwardReq, Req} = http_request_decoder:handle_request(ClientSock),
      engage_bee(ClientSock, RequestPid, RoutingKey, ForwardReq, Req, bee_srv:get_bee(self(), RoutingKey));
    _E ->
      proxy_init(ClientSock)
  after ?IDLE_TIMEOUT ->
    % We only give 30 seconds to the proxy pid to be given control of the socket
    % this is MORE than enough time for the socket to be given a chance to prepare
    % to be handled by the proxy accept request.
    send_and_terminate(ClientSock, error, ?APP_ERROR("Proxy is b0rked because of bee selection timeout in init"))
  end.

% Initiate and engage the chosen bee.
% First, we try to connect to the bee. If this succeeds, then let the rest of the app know that the
% bee has been engaged (nonblocking, through the event handler).
% Let the packet_decoder address the forwarding of the packet to the server and start the proxy loop
% If the bee cannot be reached, send an alert through the event handler that we could not reach
% the bee and try to find a new bee
engage_bee(ClientSock, RequestPid, Hostname, ForwardReq, Req, {ok, #bee{host = Host, port = Port} = Backend}) ->
  SockOpts = [  binary,
				        {active, false}
			       ],
  case gen_tcp:connect(Host, Port, SockOpts, ?CONNECT_TIMEOUT) of
    {ok, ServerSock} ->
      ?NOTIFY({bee, used, Backend}),
      % Sending raw request to bee server
      gen_tcp:send(ServerSock, ForwardReq),
      
      Timeout = timer:seconds(10),
      inet:setopts(ClientSock, [{active, false}]),
      ProxyPid = self(),
      spawn(fun() -> handle_streaming_data(ClientSock, Req, ProxyPid, Timeout) end),
      
      inet:setopts(ServerSock, [{active, once}]),
      proxy_loop(#state{
                  start_time = date_util:now_to_seconds(),
                  client_socket = ClientSock, 
                  server_socket = ServerSock, 
                  request = Req,
                  subdomain = Hostname,
                  timeout = Timeout,
                  bee = Backend}
                );
    {error, emfile} ->
      ?LOG(error, "Maximum number of sockets open. Die instead", []),
      ?NOTIFY({bee, cannot_connect, Backend}),
      send_and_terminate(
        ClientSock, 503,
        ?APP_ERROR("503 Service Unavailable")
      );
    {error,econnreset} ->
      timer:sleep(200),
      engage_bee(ClientSock, RequestPid, Hostname, ForwardReq, Req, bee_srv:get_bee(RequestPid, Hostname));
    Error ->
      ?LOG(error, "Connection to remote TCP server: ~p:~p ~p", [Host, Port, Error]),
      ?NOTIFY({bee, cannot_connect, Backend}),
      timer:sleep(200),
      engage_bee(ClientSock, RequestPid, Hostname, ForwardReq, Req, bee_srv:get_bee(RequestPid, Hostname))
  end;
engage_bee(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, ?BACKEND_TIMEOUT_MSG) ->
  ?LOG(error, "Error getting bee because of timeout: ~p", [Hostname]),
  send_and_terminate(
    ClientSock, ?BACKEND_TIMEOUT_MSG, 
    ?APP_ERROR(io_lib:format("Error: ~p", [?BACKEND_TIMEOUT_MSG]))
  );
engage_bee(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, {error, Reason}) ->
  send_and_terminate(
    ClientSock, Reason, 
    ?APP_ERROR(io_lib:format("Error on ~p: ~p", [Hostname, Reason]))
  );
engage_bee(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, Else) ->
  send_and_terminate(
    ClientSock, Else, 
    ?APP_ERROR(io_lib:format("Error on ~p: ~p", [Hostname, Else]))
  ).

% Handle all the proxy functions here
proxy_loop(#state{client_socket = CSock, server_socket = SSock} = State) ->
  receive
	  {tcp, CSock, Data} ->
      % Received data from the client
	    gen_tcp:send(SSock, Data),
	    proxy_loop(State);
  	{tcp, SSock, Data} ->
      % Received info from the server
      gen_tcp:send(CSock, Data),
      inet:setopts(SSock, [{active, once}]),
      proxy_loop(State);
    {tcp_closed, CSock} ->
  	  terminate(normal, State);
		{tcp_closed, SSock} ->
  	  terminate(normal, State);
  	{tcp_error, SSock} ->
  	  ?LOG(error, "tcp_error on server: ~p", [SSock]),
      terminate(normal, State);
    ?BACKEND_TIMEOUT_MSG ->
      % ?LOG(info, "Backend timeout message", []),
      terminate(timeout, State);
  	Msg ->
	    ?LOG(info, "~s:proxy_loop: unexpectedly recieved ~w\n", [?MODULE, Msg]),
	    proxy_loop(State)
  % If there is no activity for 2 minutes and the socket has not already closed, 
  % we'll assume that the connection is tired and should close, so we'll close it
  after 120000 ->
    ?LOG(info, "Terminating open proxy connection because of timeout", []),
    terminate(normal, State)
  end.

% Close the engage_bee client socket, but send client data first
send_and_terminate(ClientSock, Reason, Data) ->
  gen_tcp:send(ClientSock, Data),
  gen_tcp:close(ClientSock),
  exit(Reason).

% Close the connection.
% First, fetch the stats on the sockets and the elapsed_time for the bckend activity
% and notify the event chains of the closing stats. This also closes the stats activity for 
% this bee. Finally, close the two sockets and leave the process. This way we can be assured
% that the process closes itself.
terminate(Reason, #state{server_socket = SSock, client_socket = CSock, start_time = STime, bee = Backend} = _State) ->
  StatsProplist1 = [{elapsed_time, date_util:now_to_seconds() - STime}],
  StatsProplist = case inet:getstat(CSock) of
    {ok, D} -> [{socket, D}|StatsProplist1];
    _ -> StatsProplist1
  end,
  
  RealBee = bee:find_by_id(Backend#bee.id),
  ?NOTIFY({bee, ready, RealBee}),
  ?NOTIFY({bee, closing_stats, RealBee, StatsProplist}),
  gen_tcp:close(SSock), gen_tcp:close(CSock),
  exit(Reason).

% Because we want to treat the proxy as a tcp proxy, we are just going to 
% try to receive data on the client socket and pass it onto the proxy
handle_streaming_data(ClientSock, Req, From, Timeout) ->
  case gen_tcp:recv(ClientSock, 0, Timeout) of
    {ok, D} ->
      From ! {tcp, ClientSock, D},
      handle_streaming_data(ClientSock, Req, From, Timeout);
    {error, _Error} ->
      From ! {tcp_closed, ClientSock}
  end.
