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
  timeout           % timeout
}).

start_link(Req) ->
  spawn_link(fun() -> proxy_init(Req) end).

proxy_init(Req) ->
  receive
    {start, Subdomain, BalancerPid, _From} ->
      ClientSock = Req:get(socket),
      inet:setopts(ClientSock, [{packet, http}]),
      engage_backend(ClientSock, BalancerPid, Subdomain, Req, app_srv:get_backend(Subdomain))
  after 30000 ->
    ?LOG(error, "Proxy is b0rked because of timeout", []),
    Req:respond(400, [], []),
    exit(error)
  end.

engage_backend(ClientSock, BalancerPid, Hostname, Req, {ok, #backend{host = Host, port = Port} = Backend}) ->
  SockOpts = [binary, {nodelay, true},
				              {active, true}, 
				              {recbuf, ?BUFSIZ},
			                {sndbuf, ?BUFSIZ}],
  case gen_tcp:connect(Host, Port, SockOpts, 1000) of
    {ok, ServerSock} ->
      app_srv:remote_ok(Backend, self()),
      send_initial_request(ServerSock, Req),
      proxy_loop(ClientSock, ServerSock, #state{request = Req, backend = Backend});
    Error ->
      ?LOG(error, "Connection to remote TCP server: ~p:~p ~p", [Host, Port, Error]),
      app_srv:remote_error(Backend, Error),
      engage_backend(ClientSock, BalancerPid, Hostname, Req, app_srv:get_backend(Hostname))
  end;
engage_backend(_ClientSock, _BalancerPid, Hostname, Req, ?BACKEND_TIMEOUT_MSG) ->
  ?LOG(error, "Error getting backend because of timeout: ~p", [Hostname]),
  Req:respond({404, [{"Content-Type", "text/html"}], ?APP_ERROR("Something went wrong: Timeout on backend")}),
  exit(error);
engage_backend(ClientSock, _BalancerPid, Hostname, _Req, {error, Reason}) ->
  ?LOG(error, "Backend for ~p was not found: ~p", [Hostname, Reason]),
  send(ClientSock, ?APP_ERROR(io_lib:format("Error: ~p", [Reason]))),
  exit(error);
engage_backend(_ClientSock, _BalancerPid, Hostname, _Req, Else) ->
  ?LOG(info, "proxy_handler received other message for ~p: ~p", [Hostname, Else]),
  exit(error).

% Handle all the proxy here
proxy_loop(closed, closed, State) -> terminate(normal, State);
proxy_loop(CSock, SSock, #state{request = Req} = State) ->
  receive
	  {tcp, CSock, Data} ->
      % Received data from the client
	    gen_tcp:send(SSock, Data),
	    inet:setopts(CSock, [{active, once}]),
	    proxy_loop(CSock, SSock, State);
  	{tcp, SSock, Data} ->
      % Received info from the server
      send(CSock, Data),
      inet:setopts(SSock, [{active, false}, {packet, raw}]),
      case gen_tcp:recv(SSock, 1024, 500) of
        {ok, D} ->
          ?LOG(info, "More on the socket: ~p", [D]),
          send(CSock, D),
          inet:setopts(SSock, [{active, once}]),
          proxy_loop(CSock, SSock, State);
        {error, closed} -> terminate(normal, State);
        {error, timeout} -> terminate(normal, State);
        E ->
          ?LOG(info, "No more on the socket: ~p", [E]),
          terminate(normal, State)
      end;
    {close} ->
      gen_tcp:close(SSock),gen_tcp:close(CSock),
      terminate(normal, State);
  	{tcp_closed, CSock} ->
  	  ?LOG(info, "Client closed connection", []),
  	  gen_tcp:close(SSock),
  	  terminate(normal, State);
		{tcp_closed, SSock} ->
  	  ?LOG(info, "Server closed connection", []),
  	  gen_tcp:close(CSock),
  	  terminate(normal, State);
  	{tcp_error, SSock} ->
      gen_tcp:close(CSock),
      terminate(normal, State);
    ?BACKEND_TIMEOUT_MSG ->
      Req:error(404, [], []),
      terminate(timeout, State);
  	Msg ->
	    ?LOG(info, "~s:proxy_loop: unexpectedly recieved ~w\n", [?MODULE, Msg]),
	    proxy_loop(CSock, SSock, State)
  after 60000 ->
    terminate(normal, State)
  end.

terminate(Reason, #state{backend = Backend} = _State) ->
  app_srv:remote_done(Backend, self()),
  exit(Reason).

send_initial_request(ServerSock, Req) ->
  ReqHeaders = build_request_headers(Req),
  send(ServerSock, ReqHeaders).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_request_headers(Req) ->
  Path = Req:get(path),
  Body = [], %Req:recv_body(?MAX_RECV_BODY),
  RawHeaders = Req:get(headers),
  Headers = mochiweb_headers:to_list(RawHeaders),
  Method = Req:get(method),
  Version = Req:get(version),
  
  Length = length(misc_utils:to_list(Body)),
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