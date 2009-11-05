%%%-------------------------------------------------------------------
%%% File    : http_client_srv_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Oct  8 18:17:01 PDT 2009
%%%-------------------------------------------------------------------

-module (http_client_srv_sup).

-behaviour(supervisor).

-export([
  start_client/1,
  start_link/0, 
  init/1, 
  stop/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Args) -> 
  supervisor:start_child(the_proxy_srv, [Args]).

init([]) ->
  ReqSrv    = {the_http_client_srv, {http_client_srv, start_link, []}, permanent, 2000, worker, [http_client_srv]},
  ProxySrv  = {the_proxy_srv,{supervisor,start_link,[{local, the_proxy_srv}, ?MODULE, [proxy_handler]]},permanent,infinity,supervisor,[]},
  
  {ok, {{one_for_one, 5, 10}, [ReqSrv, ProxySrv]}};

init([Module]) ->
  ProxySrv = {undefined,{Module,start_link,[]},temporary,2000,worker,[]},
  {ok, {{simple_one_for_one, 5, 10}, [ProxySrv]}}.

stop(_Args) ->
  ok.