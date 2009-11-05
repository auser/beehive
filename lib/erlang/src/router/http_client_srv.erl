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
      %       {ok, ProxyPid} = spawn_link(?PROXY_HANDLER, start_link, [ClientSock]),
      %       inet:setopts(ClientSock, [{recbuf, ?BUFSIZ}, {sndbuf, ?BUFSIZ}]),
      % gen_tcp:controlling_process(ClientSock, ProxyPid),
      %       ProxyPid ! {start, ClientSock, BalancerPid},
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


% INTERNAL

% Make this dynamic-able?
% handle("beehive", Req) -> 
%   rest_srv:dispatch_requests(Req);

% handle(Subdomain, Req) ->  
%   ClientSock = Req:get(socket),
%   inet:setopts(ClientSock, [{active, once}]),
%   BalancerPid = whereis(apps),    % TODO: Fix this... please (go distributed)
%   
%   {ok, ProxyPid} = http_client_srv_sup:start_client(Req),
%   gen_tcp:controlling_process(ClientSock, ProxyPid),
%   ?LOG(info, "port_info in ~p: ~p", [?MODULE, erlang:port_info(ClientSock)]),
%   
%   ProxyPid ! {start, Subdomain, BalancerPid, ClientSock}.
%     
% parse_subdomain(HostName) ->
%   StrippedHostname = lists:takewhile(fun (C) -> C =/= $: end, HostName),
%   lists:takewhile(fun (C) -> C =/= $. end, StrippedHostname).
% 
% % HTTP
% request(Socket, Body) ->
%     case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
%         {ok, {http_request, Method, Path, Version}} ->
%             headers(Socket, {Method, Path, Version}, [], Body, 0);
%         {error, {http_error, "\r\n"}} ->
%             request(Socket, Body);
%         {error, {http_error, "\n"}} ->
%             request(Socket, Body);
%         _Other ->
%             gen_tcp:close(Socket),
%             exit(normal)
%     end.
% 
% headers(Socket, Request, Headers, _Body, ?MAX_HEADERS) ->
%   %% Too many headers sent, bad request.
%   inet:setopts(Socket, [{packet, raw}]),
%   Req = mochiweb:new_request({Socket, Request, lists:reverse(Headers)}),
%   Req:respond({400, [], []}),
%   gen_tcp:close(Socket),
%   exit(normal);
% 
% headers(Socket, Request, Headers, Body, HeaderCount) ->
%   case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
%     {ok, http_eoh} ->
%       inet:setopts(Socket, [{packet, raw}]),
%       {Request, Headers, Body};
%     {ok, {http_header, _, Name, _, Value}} ->
%       headers(Socket, Request, [{Name, Value} | Headers], Body, 1 + HeaderCount);
%     _Other ->
%       gen_tcp:close(Socket),
%       exit(normal)
%   end.
