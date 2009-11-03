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
  dispatch_requests/1
]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  Port = apps:search_for_application_value(client_port, 8080, router),
  Settings = [
    "Http client web server",
    {"Port", integer_to_list(Port)}
  ],
  printer:banner(Settings),
  
  mochiweb_http:start([ {port, Port},
                        {max, 1000000},
                        {loop, fun dispatch_requests/1}]).

dispatch_requests(Req) ->
  Host = Req:get_header_value("Host"),
  Subdomain = parse_subdomain(Host),
  handle(Subdomain, Req).

% INTERNAL

% Make this dynamic-able?
handle("beehive", Req) -> 
  rest_srv:dispatch_requests(Req);

handle(Subdomain, Req) ->  
  ClientSock = Req:get(socket),
  BalancerPid = whereis(apps),    % TODO: Fix this... please (go distributed)
  
  {_, ProxyPid} = http_client_srv_sup:start_client(Req), % spawn_link(?PROXY_HANDLER, proxy, [ClientSock]),
  gen_tcp:controlling_process(ClientSock, ProxyPid),
  
  ProxyPid ! {start, Subdomain, BalancerPid, self()}.
    
parse_subdomain(HostName) ->
  StrippedHostname = lists:takewhile(fun (C) -> C =/= $: end, HostName),
  lists:takewhile(fun (C) -> C =/= $. end, StrippedHostname).
