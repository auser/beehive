%%%-------------------------------------------------------------------
%%% File    : stats_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Stats server
%%%   TODO: Add ets table dumping
%%% Created :  Sun Nov  8 17:56:40 PST 2009
%%%-------------------------------------------------------------------

-module (stats_srv).

-include ("router.hrl").
-behaviour(gen_server2).

%% API
-export([
  start_link/0,
  backend_dump/0,
  backend_dump/1,
  backend_stat/1,
  new_backend_stat/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  backend_stats     % dict of the backends and their stats
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
backend_stat({request_begin, Key})       -> gen_server:cast(?SERVER, {backend_stat, request_begin, Key});
backend_stat({request_complete, Key})    -> gen_server:cast(?SERVER, {backend_stat, request_complete, Key});
backend_stat({elapsed_time, Key, Time})  -> gen_server:cast(?SERVER, {backend_stat, elapsed_time, Key, Time});
backend_stat({socket, Key, SocketVals})  -> gen_server:cast(?SERVER, {backend_stat, socket, Key, SocketVals}).

backend_dump(Key) -> gen_server:call(?SERVER, {backend_dump, Key}).
backend_dump()    -> gen_server:call(?SERVER, {backend_dump}).

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
handle_call({backend_dump, Name}, _From, #state{backend_stats = Dict} = State) ->
  Dict1 = dict:filter(fun(N,_) -> N =:= Name end, Dict),
  {reply, dict:to_list(Dict1), State};
  
handle_call({backend_dump}, _From, #state{backend_stats = Dict} = State) ->
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
handle_cast({backend_stat, request_begin, Key}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{current = CurrCount} = NewBackendStat, ADict} = dict_with_backend_stat(Key, Dict),
  NewBackendStat2 = NewBackendStat#backend_stat{current = CurrCount + 1},
  NewDict = dict:store(Key, NewBackendStat2, ADict),
  {noreply, State#state{backend_stats = NewDict}};
    
handle_cast({backend_stat, request_complete, Key}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{total_requests = TotReq, current = CurrCount} = NewBackendStat, ADict} = dict_with_backend_stat(Key, Dict),
  
  NewBackendStat2 = NewBackendStat#backend_stat{
    current = CurrCount - 1,
    total_requests = TotReq + 1
  },
  NewDict = dict:store(Key, NewBackendStat2, ADict),
  {noreply, State#state{backend_stats = NewDict}};
  
handle_cast({backend_stat, elapsed_time, Key, Time}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{total_time = TTime, average_req_time = AvgTime} = BackendStat, ADict} = dict_with_backend_stat(Key, Dict),
  Avg1 = (AvgTime + Time) / 2,
  Total1 = TTime + Time,
  NewBackendStat = BackendStat#backend_stat{average_req_time = Avg1, total_time = Total1},
  NewDict = dict:store(Key, NewBackendStat, ADict),
  {noreply, State#state{backend_stats = NewDict}};
  
handle_cast({backend_stat, socket, Key, SocketVals}, #state{backend_stats = Dict} = State) ->
  {#backend_stat{packet_count = CurrentPacketCount} = BackendStat, 
    ADict} = dict_with_backend_stat(Key, Dict),
  
  NewBackendStat = case proplists:get_value(recv_cnt, SocketVals) of
    undefined -> ok;
    Val -> BackendStat#backend_stat{packet_count = CurrentPacketCount + Val}
  end,
  
  NewDict = dict:store(Key, NewBackendStat, ADict),
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
    average_req_time  = 0,
    packet_count      = 0
  }.
  
dict_with_backend_stat(Key, Dict) ->
  case dict:find(Key, Dict) of
    {ok, Val} -> {Val, Dict};
    _ -> 
      NewBackendStat = new_backend_stat(),
      {NewBackendStat, dict:store(Key, NewBackendStat, Dict)}
  end.