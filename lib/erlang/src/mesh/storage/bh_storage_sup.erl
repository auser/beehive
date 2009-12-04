%%%-------------------------------------------------------------------
%%% File    : bh_storage_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Dec  2 21:09:27 PST 2009
%%%-------------------------------------------------------------------

-module (bh_storage_sup).

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
init(_Args) ->
  GitStorageSrv = {the_storage_server, {bh_storage_srv, start_link, []}, permanent, 2000, worker, dynamic},
  _GitServer  = {the_git_server,{bh_git_srv, start_link,[]}, permanent, 2000, worker, dynamic},
  
  {ok,{{one_for_one,5,10}, [GitStorageSrv]}}.

%%====================================================================
%% Internal functions
%%====================================================================