%%%-------------------------------------------------------------------
%%% File    : tcp_socket_server.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Nov  6 11:11:36 PST 2009
%%%-------------------------------------------------------------------

-module (tcp_socket_server).

-include ("beehive.hrl").
-include ("http.hrl").
-include ("common.hrl").

%% API
-export([
  start_link/0,
  init/0,
  init/1,
  init_accept/1
]).

-define (SUP, tcp_socket_server_sup).

%%====================================================================
%% API
%%====================================================================
% TODO: Move to proc_lib:start_link
start_link()          -> init().
% Start listening on the application port
% this can be modified with a config file or an environment variable
% e.g. BEEHIVE_CLIENT_PORT=80
init()                ->
  init(config:search_for_application_value(client_port, 8080)).
init(LocalPort) ->
  Pid = spawn_link(?MODULE, init_accept, [LocalPort]),
  {ok, Pid}.

% accept responses on the port given by the application configuration
init_accept(LPort) ->
  SockOpts = [binary, {backlog, 256}, {nodelay, false},
              {reuseaddr, true},{active, false}],
	case gen_tcp:listen(LPort, SockOpts) of
	  {ok, ListenSocket} ->
	    accept(ListenSocket);
	  Error ->
	    ?LOG(error,
                 "There was an error listening to the socket for port ~p: ~p",
                 [LPort, Error]),
	    error
	end.

%% Accept a new socket connection to the server. If the socket
%% connection is successful, then move on and decode the socket type
%% (for now, we'll assume it's an http request) and pass it on to the
%% proxy handler in a separate process. Move on to accept the next
%% request in this process so that we are never blocking the socket
%% server
accept(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, ClientSock} ->
      spawn(fun() -> pass_on_to_proxy(ClientSock) end),
	    accept(LSock);
    Error ->
      ?LOG(error, "There was an error accepting the socket ~p: ~p",
           [LSock, Error]),
      exit(error)
  end.

%% Take the socket and decode the routing key from the packet. For
%% http, this means accept enough on the request to pull off the
%% headers and decode the 'Host' parameter (or other routing parameter
%% defined by routing_parameter tphen starting a proxy handler proxy
%% process and finally passing the socket to the proxy handler process
pass_on_to_proxy(ClientSock) ->
  %% Chose here the type of response... for now, it'll just be http,
  %% but in the future... maybe tcp/udp?
  {ok, ProxyPid} = ?SUP:start_client(ClientSock),
  gen_tcp:controlling_process(ClientSock, ProxyPid),
  ProxyPid ! {start, ClientSock, ProxyPid}.
