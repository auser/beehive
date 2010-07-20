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
-export([start_link/0,start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Args), 
  {I, 
    {I, start_link, lists:flatten([proplists:get_value(I, Args, [])])}, 
    permanent, 5000, Type, [I]
  }).
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() -> start_link([]).
start_link(Args) ->
  % Start os_mon
  application:start(os_mon),
  
  % Seed the random number generator
  random:seed(now()),
  
  % Start the whole chain
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
  RestServer    = ?CHILD(rest_server_sup, worker, Args),
  BeehiveRouter = ?CHILD(beehive_router_sup, supervisor, Args),
  % Setup beehive
  NodeType = config:search_for_application_value(node_type, beehive_router),  
  ShouldRunRouter     = case NodeType of
    beehive_router -> true;
    _ -> false
  end,
  ShouldRunRestServer = config:search_for_application_value(run_rest_server, true),
  
  Children = lists:flatten([
    % For the distributed database
    ?CHILD(beehive_db_srv, worker, Args),
    % Manage the nodes and apps
    ?CHILD(node_manager, worker, Args),
    ?CHILD(app_manager, worker, Args),
    % Start the event manager
    ?CHILD(event_manager, worker, Args),
    ?CHILD(babysitter, worker, Args),
    ?CHILD(app_handler, worker, Args),
    % Storage stuff
    ?CHILD(beehive_storage_srv, worker, Args),
    ?CHILD(beehive_git_srv, worker, Args),
    % Rest server, should we run it?
    ?IF(ShouldRunRouter, BeehiveRouter, []),    
    ?IF(ShouldRunRestServer, RestServer, [])
  ]),
  
  {ok,{{one_for_one,5,10}, Children}}.
  
%%====================================================================
%% Internal functions
%%====================================================================