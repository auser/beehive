%%%-------------------------------------------------------------------
%%% File    : router_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Oct 25 13:09:07 PDT 2009
%%%-------------------------------------------------------------------

-module (router_sup).
-include ("router.hrl").
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
init(_Args) ->
  % spawn(fun() -> fs_srv:start() end),
  % backend_srv_sup:start_link(?KVSTORE).
  AppEventManager = {the_app_event_manager, {?EVENT_MANAGER, start_link, []}, permanent, 2000, worker, dynamic},
  AppSrv = {the_backend_srv_sup, {backend_srv_sup, start_link, [dict_store]}, permanent,2000,worker,dynamic},
  HttpCl = {the_socket_server, {socket_server_sup, start_link, []}, permanent,2000,worker,dynamic},
  StatSrv = {the_stats_srv, {stats_srv, start_link, []}, permanent,2000,worker,dynamic},
  
  {ok,{{one_for_one,5,10}, [AppEventManager,AppSrv,HttpCl, StatSrv]}}.

%%====================================================================
%% Internal functions
%%====================================================================