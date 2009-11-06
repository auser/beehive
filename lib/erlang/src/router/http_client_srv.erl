%%%-------------------------------------------------------------------
%%% File    : http_client_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Oct  8 18:16:25 PDT 2009
%%%-------------------------------------------------------------------

-module (http_client_srv).

-include ("router.hrl").
-include ("http.hrl").
-include ("common.hrl").

%% API
-export([
  start_link/0,
  init/0,init/1,init/2,
  init_accept/2
]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
% start_link() ->
%   Port = apps:search_for_application_value(client_port, 8080, router),
%   Settings = [
%     "Http client web server",
%     {"Port", integer_to_list(Port)}
%   ],
%   printer:banner(Settings),
%   
%   mochiweb_http:start([ {port, Port},
%                         {max, 1000000},
%                         {loop, fun dispatch_requests/1}]).
% 
% dispatch_requests(Req) ->
%   ?LOG(info, "should close: ~p", [Req:should_close()]),
%   Host = Req:get_header_value("Host"),
%   Subdomain = parse_subdomain(Host),
%   handle(Subdomain, Req).

start_link()          -> init().
% Start listening on the application port
init()                -> init(apps:search_for_application_value(client_port, 8080, router)).
init(LocalPort)       -> init(LocalPort, self()).
init(LocalPort, BalancerPid) -> 
  Pid = spawn_link(?MODULE, init_accept, [LocalPort, BalancerPid]),
  {ok, Pid}.

% 
init_accept(LPort, BalancerPid) ->
  SockOpts = [binary, {backlog, 256}, {nodelay, true},{reuseaddr, true}, {active, false}],
	case gen_tcp:listen(LPort, SockOpts) of
	  {ok, ListenSocket} -> 
	    accept(ListenSocket, BalancerPid);
	  Error ->
	    ?LOG(error, "There was an error listening to the socket for port ~p: ~p", [LPort, Error]),
	    error
	end.

accept(LSock, BalancerPid) ->
  case gen_tcp:accept(LSock) of
    {ok, ClientSock} ->
      spawn(fun() ->
        {ok, ProxyPid} = http_client_srv_sup:start_client(ClientSock),
        gen_tcp:controlling_process(ClientSock, ProxyPid),      
        ProxyPid ! {start, ClientSock, BalancerPid}
      end),
	    accept(LSock, BalancerPid);
    Error ->
      ?LOG(error, "There was an error accepting the socket ~p: ~p", [LSock, Error]),
      exit(error)
  end.
