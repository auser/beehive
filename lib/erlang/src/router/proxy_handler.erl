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
  backend,          % Backend
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
% routing key required. If this can be found, then start engaging the backend.
% If this takes more than IDLE_TIMEOUT seconds, then exit with an error.
proxy_init(ClientSock) ->
  receive
    {start, ClientSock, RequestPid} ->
      {ok, RoutingKey, ForwardReq, Req} = http_request:handle_request(ClientSock),
      engage_backend(ClientSock, RequestPid, RoutingKey, ForwardReq, Req, backend_srv:get_backend(self(), RoutingKey));
    _E ->
      proxy_init(ClientSock)
  after ?IDLE_TIMEOUT ->
    % We only give 30 seconds to the proxy pid to be given control of the socket
    % this is MORE than enough time for the socket to be given a chance to prepare
    % to be handled by the proxy accept request.
    send_and_terminate(ClientSock, error, ?APP_ERROR("Proxy is b0rked because of backend selection timeout in init"))
  end.

% Initiate and engage the chosen backend.
% First, we try to connect to the backend. If this succeeds, then let the rest of the app know that the
% backend has been engaged (nonblocking, through the event handler).
% Let the packet_decoder address the forwarding of the packet to the server and start the proxy loop
% If the backend cannot be reached, send an alert through the event handler that we could not reach
% the backend and try to find a new backend
engage_backend(ClientSock, RequestPid, Hostname, ForwardReq, Req, {ok, #backend{host = Host, port = Port} = Backend}) ->
  SockOpts = [  binary, 
                {nodelay, true},
				        {active, once},
				        {packet, 0},
				        {reuseaddr, true}
			       ],
  case gen_tcp:connect(Host, Port, SockOpts) of
    {ok, ServerSock} ->
      ?NOTIFY({backend, used, Backend}),
      % Sending raw request to backend server
      gen_tcp:send(ServerSock, ForwardReq),
      
      inet:setopts(ClientSock, [{active, false}]),
      ProxyPid = self(),
      spawn(fun() -> handle_streaming_data(ClientSock, Req, ProxyPid) end),
      
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
      engage_backend(ClientSock, RequestPid, Hostname, ForwardReq, Req, backend_srv:get_backend(RequestPid, Hostname))
  end;
engage_backend(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, ?BACKEND_TIMEOUT_MSG) ->
  ?LOG(error, "Error getting backend because of timeout: ~p", [Hostname]),
  send_and_terminate(
    ClientSock, ?BACKEND_TIMEOUT_MSG, 
    ?APP_ERROR(io_lib:format("Error: ~p", [?BACKEND_TIMEOUT_MSG]))
  );
engage_backend(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, {error, Reason}) ->
  send_and_terminate(
    ClientSock, Reason, 
    ?APP_ERROR(io_lib:format("Error on ~p: ~p", [Hostname, Reason]))
  );
engage_backend(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, Else) ->
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
      % Don't see much of a way around this... but there has got to be a better way
      % Try for a half of a second to passivly receive more data on the server socket
      % if there is more data, then we'll assume there is a lot more data, so send
      % it and then continue the proxy, otherwise we'll assume that the proxy is dead
      % and we should terminate the proxy
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

% Close the engage_backend client socket, but send client data first
send_and_terminate(ClientSock, Reason, Data) ->
  gen_tcp:send(ClientSock, Data),
  gen_tcp:close(ClientSock),
  exit(Reason).

% Close the connection.
% First, fetch the stats on the sockets and the elapsed_time for the bckend activity
% and notify the event chains of the closing stats. This also closes the stats activity for 
% this backend. Finally, close the two sockets and leave the process. This way we can be assured
% that the process closes itself.
terminate(Reason, #state{server_socket = SSock, client_socket = CSock, start_time = STime, backend = Backend} = _State) ->
  StatsProplist1 = [{elapsed_time, date_util:now_to_seconds() - STime}],
  StatsProplist = case inet:getstat(CSock) of
    {ok, D} -> [{socket, D}|StatsProplist1];
    _ -> StatsProplist1
  end,

  ?NOTIFY({backend, closing_stats, Backend, StatsProplist}),
  gen_tcp:close(SSock), gen_tcp:close(CSock),
  exit(Reason).

% Because we want to treat the proxy as a tcp proxy, we are just going to 
% try to receive data on the client socket and pass it onto the proxy
handle_streaming_data(ClientSock, Req, From) ->
  case gen_tcp:recv(ClientSock, 0) of
    {ok, D} ->
      From ! {tcp, ClientSock, D},
      handle_streaming_data(ClientSock, Req, From);
    {error, _Error} ->
      From ! {tcp_closed, ClientSock}
  end.