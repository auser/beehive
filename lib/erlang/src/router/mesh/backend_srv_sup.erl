%%%-------------------------------------------------------------------
%%% File    : backend_srv_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Oct  8 02:09:02 PDT 2009
%%%-------------------------------------------------------------------

-module (backend_srv_sup).
-include ("router.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

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
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
  AppSrv  = {the_backend_srv,{backend_srv, start_link,[]}, permanent,2000,worker,dynamic},
  AppManagerSrv  = {the_app_manager,{app_manager, start_link,[]}, permanent, 2000, worker, dynamic},
  BHApps  = {the_beehive,{rest_server, start_link,[]}, permanent,2000,worker,dynamic},
  
  AppsToStart = [AppSrv, AppManagerSrv],
  
  RunBH = apps:search_for_application_value(run_beehive, true, router),
  io:format("Running beehive: ~p (~p)~n", [RunBH, application:get_all_env()]),
  AppsToStart2 = case apps:search_for_application_value(run_beehive, true, router) of
    true -> [BHApps|AppsToStart];
    false -> AppsToStart
  end,
  
  {ok,{{one_for_one,5,10}, AppsToStart2}};
  
init([start_module, Module]) ->
  ModSrv = {undefined,{Module,start_link,[]},temporary,2000,worker,[]},
  {ok, {{simple_one_for_one, 5, 10}, [ModSrv]}}.

%%====================================================================
%% Internal functions
%%====================================================================