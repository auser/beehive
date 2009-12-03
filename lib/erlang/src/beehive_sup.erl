%%%-------------------------------------------------------------------
%%% File    : beehive_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Oct 25 13:09:07 PDT 2009
%%%-------------------------------------------------------------------

-module (beehive_sup).
-include ("beehive.hrl").
-behaviour(supervisor).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

%% API
-export([start_link/1]).
-export ([start_internal_supervisors/1]).

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
  case supervisor:start_link({local, ?SERVER}, ?MODULE, Args) of
    {ok, Pid} ->
      {ok, Pid};
    Else ->
      io:format("There was an error: ~p~n", [Else])
  end.

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
  NodeManager = {the_node_manager, {node_manager, start_link, []}, permanent, 2000, worker, dynamic},
  EventManager = {the_app_event_manager, {?EVENT_MANAGER, start_link, []}, permanent, 2000, worker, dynamic},
  RoleSupervisors = {the_roles, {?MODULE, start_internal_supervisors, [Args]}, permanent, 2000, worker, dynamic},
  
  {ok,{{one_for_one,5,10}, [EventManager, NodeManager, RoleSupervisors]}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_internal_supervisors(Args) ->
  case config:search_for_application_value(node_type, router, beehive) of
    node -> supervisor:start_link({local, bh_node_sup}, bh_node_sup, Args);
    storage -> supervisor:start_link({local, bh_storage_sup}, bh_storage_sup, Args);
    router -> supervisor:start_link({local, bh_router_sup}, bh_router_sup, Args)
  end.