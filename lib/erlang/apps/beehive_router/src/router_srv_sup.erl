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

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

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
  % gen_cluster:start_link({local, Name}, Mod, RealArgs, Opts);
  AppSrv  = {router_srv,{gen_cluster, start_link, [{local, router_srv}, router_srv, [], []]}, permanent,2000,worker,dynamic},
  RestServer  = ?CHILD(rest_server_sup, worker),
  
  ShouldRunRestServer = config:search_for_application_value(run_rest_server, true, beehive),
  
  Children = lists:flatten([
    AppSrv,
    ?IF(ShouldRunRestServer, RestServer, [])
  ]),
  
  {ok,{{one_for_one,5,10}, Children}}.
%%====================================================================
%% Internal functions
%%====================================================================