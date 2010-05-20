%%%-------------------------------------------------------------------
%%% File    : beehive_node_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Dec  2 20:11:42 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_node_sup).

-include ("beehive.hrl").
-behaviour(supervisor).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

%% API
-export([start_link/1]).

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
  Children = lists:flatten([
    ?CHILD(app_handler, worker),
    ?CHILD(babysitter, worker)
  ]),
  
  {ok,{{one_for_one,5,10}, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================