%%%-------------------------------------------------------------------
%%% File    : proxy_handler.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Tue Oct 13 20:24:21 PDT 2009
%%%-------------------------------------------------------------------

-module (proxy_handler).

-include ("router.hrl").
-include ("common.hrl").
-include ("http.hrl").
-include ("beehive.hrl").

-export ([
  start_link/1
]).

-export ([terminate1/2]).

-record(state, {
  routing_key,          % subdomain of the app to look for
  host,                 % host
  port,                 % port
  bee,                  % Backend
  request,              % client request
  start_time,           % time proxy started
  client_socket,        % client socket
  server_socket,        % server socket
  client_pid,           % client listening pid
  server_pid,           % server listening pid
  timeout               % timeout
}).

%% Start the proxy by spawning off an init with the socket.
%% Compatible with the supervisor behaviour
start_link(ClientSock) ->
  Fun = fun() -> proxy_init(ClientSock) end,
  Pid = ?BENCHMARK_LOG("------Spawn-linking proxy_init------",
                       erlang, spawn_link, [Fun]),
  {ok, Pid}.

%% Receive the initial request and socket. Use the packet_decoder (http
%% for now) to decode the routing key required. If this can be found,
%% then start engaging the bee.  If this takes more than IDLE_TIMEOUT
%% seconds, then exit with an error.
proxy_init(ClientSock) ->
  receive
    {start, ClientSock, RequestPid} ->
      {ok, RoutingKey, ForwardReq, Req} =
        ?BENCHMARK_LOG("Handling request", http_request_decoder,
                       handle_request, [ClientSock]),
      GetBee = ?BENCHMARK_LOG("Getting bee for routing key",
                              bee_store, get_bee, [RoutingKey]),
      case RoutingKey of
        default -> ok;
        _ ->
          ?LOG(debug, "bee_store:get_bee(~s): ~p", [RoutingKey, GetBee])
      end,
      engage_bee(ClientSock, RequestPid, RoutingKey, ForwardReq, Req, GetBee);
    _E ->
      proxy_init(ClientSock)
  after ?IDLE_TIMEOUT ->
    %% We only give 30 seconds to the proxy pid to be given control of
    %% the socket this is MORE than enough time for the socket to be
    %% given a chance to prepare to be handled by the proxy accept
    %% request.
    send_and_terminate(ClientSock, error,
                       ?APP_ERROR(503, "Proxy is b0rked because of bee selection timeout in init"))
  end.

%% Initiate and engage the chosen bee.  First, we try to connect to
%% the bee. If this succeeds, then let the rest of the app know that
%% the bee has been engaged (nonblocking, through the event handler).
%% Let the packet_decoder address the forwarding of the packet to the
%% server and start the proxy loop If the bee cannot be reached, send
%% an alert through the event handler that we could not reach the bee
%% and try to find a new bee
engage_bee(ClientSock,
           _RequestPid,
           RoutingKey,
           ForwardReq,
           Req,
           {ok, #bee{host = Host, port = Port} = Bee, ServerSock}) ->
  ?NOTIFY({bee, used, Bee}),
  % Sending raw request to bee server
  gen_tcp:send(ServerSock, ForwardReq),

  Timeout = timer:seconds(10),
  ProxyPid = self(),

  State = #state{
      start_time = date_util:now_to_seconds(),
      client_socket = ClientSock,
      server_socket = ServerSock,
      request = Req,
      routing_key = RoutingKey,
      host = Host,
      port = Port,
      timeout = Timeout,
      bee = Bee},

  ClientPid =
    spawn(fun() -> handle_streaming_data(client, ProxyPid, State) end),
  ServerPid =
    spawn(fun() -> handle_streaming_data(server, ProxyPid, State) end),

  NewState = State#state{server_pid = ServerPid, client_pid = ClientPid},

  proxy_loop(NewState);
engage_bee(ClientSock,
           _RequestPid,
           Hostname,
           _ForwardReq,
           _Req,
           {error, Reason}) ->
  erlang:display({engage_bee,error,Reason}),
  send_and_terminate(
    ClientSock, Reason,
    ?APP_ERROR(404, io_lib:format("Error on ~p: ~p", [Hostname, Reason]))
  );
engage_bee(ClientSock, _RequestPid, Hostname, _ForwardReq, _Req, Else) ->
  send_and_terminate(
    ClientSock, Else,
    ?APP_ERROR(404, io_lib:format("Error on ~p: ~p", [Hostname, Else]))
  ).

% Handle all the proxy functions here
proxy_loop(#state{client_socket = CSock, server_socket = SSock,
                  server_pid = SPid, client_pid = CPid} = State) ->
  receive
    {tcp, CSock, Data} ->
                                                % Received data from the client
      ?BENCHMARK_LOG("Sending data to Server", gen_tcp, send, [SSock, Data]),
      proxy_loop(State);
    {tcp, SSock, Data} ->
      % Received info from the server
      ?BENCHMARK_LOG("Sending data to client", gen_tcp, send, [CSock, Data]),
      proxy_loop(State);
    {tcp_closed, SPid, _Sock} ->
      terminate(normal, State);
    {tcp_closed, CPid, _Sock} ->
      terminate(normal, State);
    {tcp_error, SSock} ->
      ?LOG(error, "tcp_error on server: ~p", [SSock]),
      terminate(normal, State);
    bee_timeout -> terminate(timeout, State);
    _Else -> proxy_loop(State)
  %% If there is no activity for a while and the socket has not already
  %% closed, we'll assume that the connection is tired and should
  %% close, so we'll close it
  after 3000 ->
    terminate(normal, State)
  end.

%% Close the engage_bee client socket, but send client data first
send_and_terminate(ClientSock, Reason, Data) ->
  gen_tcp:send(ClientSock, Data),
  gen_tcp:close(ClientSock),
  exit(Reason).

%% We'll be mindful of the state of the proxy

%% Close the connection.  First, fetch the stats on the sockets and
%% the elapsed_time for the bckend activity and notify the event
%% chains of the closing stats. This also closes the stats activity
%% for this bee. Finally, close the two sockets and leave the
%% process. This way we can be assured that the process closes itself.
terminate(Reason, State) ->
  ?BENCHMARK_LOG("------Terminating proxy------",
                 ?MODULE, terminate1, [Reason, State]).

terminate1(Reason, #state{server_socket = SSock, client_socket = CSock,
                          start_time = STime, bee = Bee} = _State) ->
  StatsProplist1 = [{elapsed_time, date_util:now_to_seconds() - STime}],
  StatsProplist = case inet:getstat(CSock) of
    {ok, D} -> [{socket, D}|StatsProplist1];
    _ -> StatsProplist1
  end,

  %% A bee would only not exist if the bee is a 'fake' bee as in one
  %% constructed for the 'base'
  RealBee = case bees:find_by_id(Bee#bee.id) of
    [] -> Bee;
    RealBee1 -> RealBee1
  end,

  ?NOTIFY({bee, ready, RealBee}),
  ?NOTIFY({bee, closing_stats, RealBee, StatsProplist}),
  gen_tcp:close(SSock), gen_tcp:close(CSock),
  exit(Reason).

%% Because we want to treat the proxy as a tcp proxy, we are just going to
%% try to receive data on the client socket and pass it onto the proxy
handle_streaming_data(client, From,
                      #state{client_socket = CSock,
                             timeout = Timeout} = State) ->
  case gen_tcp:recv(CSock, 0, Timeout) of
    {ok, D} ->
      From ! {tcp, CSock, D},
      handle_streaming_data(client, From, State);
    {error, _Error} ->
      From ! {tcp_closed, self(), CSock}
  end;
handle_streaming_data(server, From,
                      #state{server_socket = SSock,
                             timeout = Timeout} = State) ->
  case gen_tcp:recv(SSock, 0, Timeout) of
    {ok, D} ->
      From ! {tcp, SSock, D},
      handle_streaming_data(server, From, State);
    {error, _Error} ->
      From ! {tcp_closed, self(), SSock}
  end.
