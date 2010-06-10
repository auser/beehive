 %%%-------------------------------------------------------------------
%%% File    : http_request_decoder.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Nov 17 17:52:40 PST 2009
%%%-------------------------------------------------------------------

-module (http_request_decoder).

-include ("router.hrl").

%% API
-export([
  handle_request/1
]).
%% TEST
-export ([
  parse_route_from_request/2
]).

% Take the connecting socket and handle the request. Get the request up until the end of the headers
% Take off the 'Host' parameter (or other sepcified parameter) from the header and return the 
% routing key and the full request to the calling process
handle_request(ClientSock) ->
  Req = beehive_request:new(ClientSock),
  
  RoutingParameter = misc_utils:to_atom(config:search_for_application_value(routing_parameter, "Host", beehive_router)),
  HeaderVal = mochiweb_headers:get_value(RoutingParameter, Req:get(headers)),
  
  BaseDomain = config:search_for_application_value(domain, undefined, beehive_router),
  Subdomain = parse_route_from_request(HeaderVal, BaseDomain),
  ForwardReq = build_request_headers(Req),
  {ok, Subdomain, ForwardReq, Req}.
  
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_request_headers(Req) ->  
  {ok, Hostname} = inet:gethostname(),
    
  Headers = mochiweb_headers:to_list(Req:get(headers)),
  Method = Req:get(method),
  Path = Req:get(raw_path),
  Version = Req:get(version),
  
  NewHeaders = replace_values_in_prolists(
      [
        {'X-Forwarded-For', Hostname}
      ],
      Headers),

  [
    misc_utils:to_list(Method), " ", Path, " HTTP/", version(Version), <<"\r\n">> |
    headers_to_list(NewHeaders)
  ].
      
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

% HTTP
% We strip off the port, just in case
%%-------------------------------------------------------------------
%% @spec (Hostname, BaseDomain) ->    Domain
%% @doc Parse the domain from the base domain
%%  Take off the domain
%%      
%% @end
%%-------------------------------------------------------------------
parse_route_from_request(undefined, _) -> base;
parse_route_from_request(HostName, undefined) -> parse_route_from_request_without_base_domain(HostName);
parse_route_from_request(HostName, BaseDomain) ->
  [NoPortHostname|_] = string:tokens(HostName, ":"),
  O = string:tokens(NoPortHostname, "."),
  T = string:tokens(BaseDomain, "."),
  
  case lists:subtract(O, T) of
    [] -> base; % The requested resource is no different from the basedomain
    List -> List
  end.
  
parse_route_from_request_without_base_domain(HostName) ->
  [NoPortHostname|_] = string:tokens(HostName, ":"),
  O = string:tokens(NoPortHostname, "."),
  parse_route_from_request_without_base_domain1(O, []).

parse_route_from_request_without_base_domain1([], _Acc)     -> base;
parse_route_from_request_without_base_domain1(["localhost"], Acc) ->  
  parse_route_from_request_without_base_domain2(Acc);
  
parse_route_from_request_without_base_domain1(["com"], Acc) -> parse_route_from_request_without_base_domain2(Acc);
parse_route_from_request_without_base_domain1(["org"], Acc) -> parse_route_from_request_without_base_domain2(Acc);
parse_route_from_request_without_base_domain1(["net"], Acc) -> parse_route_from_request_without_base_domain2(Acc);
parse_route_from_request_without_base_domain1([H|Rest], Acc) -> 
  parse_route_from_request_without_base_domain1(Rest, [H|Acc]).

parse_route_from_request_without_base_domain2([]) -> base;
parse_route_from_request_without_base_domain2(List) ->
  [H|_Rest] = lists:reverse(List), [H].
