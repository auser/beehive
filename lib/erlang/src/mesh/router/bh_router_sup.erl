%%%-------------------------------------------------------------------
%%% File    : bh_router_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Dec  2 20:13:46 PST 2009
%%%-------------------------------------------------------------------

-module (bh_router_sup).

-include ("beehive.hrl").
-behaviour(supervisor).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init(Args) ->
  AppSrv = {the_router_srv_sup, {router_srv_sup, start_link, Args}, permanent,2000,worker,dynamic},
  HttpCl = {the_socket_server, {socket_server_sup, start_link, Args}, permanent,2000,worker,dynamic},
  BeeStatSrv = {the_bh_bee_stats_srv, {bh_bee_stats_srv, start_link, Args}, permanent,2000,worker,dynamic},
  AppManagerSrv  = {the_app_manager,{app_manager, start_link, Args}, permanent, 2000, worker, dynamic},
  
  AppsToStart = [AppManagerSrv,AppSrv,BeeStatSrv,HttpCl],
  
  {ok,{{one_for_one,5,10}, AppsToStart}}.

%%====================================================================
%% Internal functions
%%====================================================================