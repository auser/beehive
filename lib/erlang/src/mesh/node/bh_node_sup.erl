%%%-------------------------------------------------------------------
%%% File    : bh_node_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Dec  2 20:11:42 PST 2009
%%%-------------------------------------------------------------------

-module (bh_node_sup).

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
  AppHandler  = {the_app_handler,{app_handler, start_link,[]}, permanent, 2000, worker, dynamic},
  BabySitter  = {the_babysitter,{babysitter_app, start,[[], Args]}, permanent, 2000, worker, dynamic},
  
  AppsToStart =[AppHandler, BabySitter],
  
  {ok,{{one_for_one,5,10}, AppsToStart}}.

%%====================================================================
%% Internal functions
%%====================================================================