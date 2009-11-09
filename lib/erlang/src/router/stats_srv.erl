%%%-------------------------------------------------------------------
%%% File    : stats_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Stats server
%%%   TODO: Add ets table dumping
%%% Created :  Sun Nov  8 17:56:40 PST 2009
%%%-------------------------------------------------------------------

-module (stats_srv).
-behaviour(gen_server2).

%% API
-export([
  start_link/0,
  dump/0,
  backend_stat/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (backend_stat, {
  total_requests,   % total requests for the backend
  current,          % current number of requests
  total_time,       % total active time
  average_req_time, % average request time
  % packets
  packet_count      % total packet counts
}).

-record(state, {
  backend_stats     % dict of the backends and their stats
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
backend_stat({request_begin, Name})       -> gen_server:cast(?SERVER, {backend_stat, request_begin, Name});
backend_stat({request_complete, Name})    -> gen_server:cast(?SERVER, {backend_stat, request_complete, Name});
backend_stat({elapsed_time, Name, Time})  -> gen_server:cast(?SERVER, {backend_stat, elapsed_time, Name, Time});
backend_stat({socket, Name, SocketVals})  -> gen_server:cast(?SERVER, {backend_stat, socket, Name, SocketVals}).

dump() ->
  gen_server:call(?SERVER, {dump}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  process_flag(trap_exit, true),
  
  % Opts = [named_table, ordered_set],
  % ets:new(backend_stat_total, Opts),
  
  State = #state{ backend_stats = dict:new() },
  
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({dump}, _From, #state{backend_stats = Dict} = State) ->
  Dict1 = dict:filter(fun(_,_) -> true end, Dict),
  {reply, dict:to_list(Dict1), State};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({backend_stat, request_begin, Name}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{total_requests = TotReq, current = CurrCount} = NewBackendStat, ADict} = dict_with_backend_stat(Name, Dict),
  NewBackendStat2 = NewBackendStat#backend_stat{total_requests = TotReq + 1, current = CurrCount + 1},
  NewDict = dict:store(Name, NewBackendStat2, ADict),
  {noreply, State#state{backend_stats = NewDict}};
    
handle_cast({backend_stat, request_complete, Name}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{total_requests = TotReq, current = CurrCount} = NewBackendStat, ADict} = dict_with_backend_stat(Name, Dict),
  
  NewBackendStat2 = NewBackendStat#backend_stat{
    current = CurrCount - 1,
    total_requests = TotReq + 1
  },
  NewDict = dict:store(Name, NewBackendStat2, ADict),
  {noreply, State#state{backend_stats = NewDict}};
  
handle_cast({backend_stat, socket, Name, SocketVals}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{packet_count = CurrentPacketCount} = NewBackendStat, 
    ADict} = dict_with_backend_stat(Name, Dict),
  
  NewBackendStat2 = case proplists:get_value(packet_count, SocketVals) of
    {ok, Val} -> NewBackendStat#backend_stat{packet_count = CurrentPacketCount + Val};
    _ -> NewBackendStat
  end,
  
  NewDict = dict:store(Name, NewBackendStat2, ADict),
  {noreply, State#state{backend_stats = NewDict}};
  
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

new_backend_stat() ->
  #backend_stat{
    total_requests    = 0,
    current           = 0,
    total_time        = 0,
    average_req_time  = gb_trees:empty(),
    packet_count      = 0
  }.
  
dict_with_backend_stat(Name, Dict) ->
  case dict:find(Name, Dict) of
    {ok, Val} -> 
      {Val, Dict};
    _ -> 
      NewBackendStat = new_backend_stat(),
      {NewBackendStat, dict:store(Name, NewBackendStat, Dict)}
  end.