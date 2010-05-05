%%%-------------------------------------------------------------------
%%% File    : router_srv_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Oct  8 02:09:02 PDT 2009
%%%-------------------------------------------------------------------

-module (router_srv_sup).
-include ("beehive.hrl").
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
init([start_module, Module]) ->
  ModSrv = {undefined,{Module,start_link,[]},temporary,2000,worker,[]},
  {ok, {{simple_one_for_one, 5, 10}, [ModSrv]}};

init(Args) ->
  % gen_cluster:start_link({local, Name}, Mod, RealArgs, Opts);
  AppSrv  = {the_router_srv,{gen_cluster, start_link, [{local, router_srv}, router_srv, Args, []]}, permanent,2000,worker,dynamic},
  BHApps  = {the_beehive,{rest_server, start_link, Args}, permanent,2000,worker,dynamic},
  
  AppsToStart = [AppSrv],
  
  AppsToStart2 = case config:search_for_application_value(run_rest_server, true, beehive) of
    true -> [BHApps|AppsToStart];
    false -> AppsToStart
  end,
  
  {ok,{{one_for_one,5,10}, AppsToStart2}}.
%%====================================================================
%% Internal functions
%%====================================================================