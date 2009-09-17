%%%-------------------------------------------------------------------
%%% File    : app_registry_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Sep  1 12:12:32 PDT 2009
%%%-------------------------------------------------------------------

-module (app_registry_srv).
-include ("beehive.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export ([find_application/1, register_application/1, list_applications/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
find_application(AppName) -> gen_server:call(?SERVER, {find_application, AppName}).
register_application(Args) -> gen_server:call(?SERVER, {register_application, Args}).
list_applications() -> gen_server:call(?SERVER, {list_local_applications}).
	
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> start_link([]).

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
  process_flag(trap_exit, true),
  
	RunningApplications = find_local_applications(),
  
  {ok, ets:new(?MODULE,RunningApplications)}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register_application, Args}, _From, State) ->
  Reply = handle_register_application(Args, State),
  {reply, Reply, State};
handle_call({find_application, AppName}, _From, State) ->
	Reply = handle_find_application(AppName, State),
	{reply, Reply, State};
handle_call({list_local_applications}, _From, State) ->
  Reply = handle_list_local_applications(State),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%====================================================================
%% HANDLERS
%%====================================================================
handle_register_application(Args, Table) ->
  Name = proplists:get_value(name, Args),
  Host = proplists:get_value(host, Args),
  Port = proplists:get_value(port, Args),
    
  % app_registry_client:start_and_link("getbeehive.com", 5001).
  % TODO: Allow for multiple hosts
  case ets:lookup(Table, Name) of
    [] ->
      ets:insert(Table, {Name, [{host, Host}, {port, Port}]}),
      {registered, Name};
    [{Name, _CurrHosts}] ->
      {already_registered, Name}
  end.
  
handle_find_application(_AppName, _State) ->
	none.
	
handle_list_local_applications(Table) ->
  ets:tab2list(Table).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------  
find_local_applications() ->
  ?INFO("Apps: ~p~n", [self()]),
  app_discovery:discover_local_apps().
