%%%-------------------------------------------------------------------
%%% File    : bh_bee_stats_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Dec 13 20:51:04 PST 2009
%%%-------------------------------------------------------------------

-module (bh_bee_stats_srv).

-include ("beehive.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  bee_dump/0,
  bee_dump/1,
  bee_stat/1,
  new_bee_stat/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  bee_stats     % dict of the bees and their stats
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
bee_stat({request_begin, Key})       -> gen_server:cast(?SERVER, {bee_stat, request_begin, Key});
bee_stat({request_complete, Key})    -> gen_server:cast(?SERVER, {bee_stat, request_complete, Key});
bee_stat({elapsed_time, Key, Time})  -> gen_server:cast(?SERVER, {bee_stat, elapsed_time, Key, Time});
bee_stat({socket, Key, SocketVals})  -> gen_server:cast(?SERVER, {bee_stat, socket, Key, SocketVals}).

bee_dump(Key) -> gen_server:call(?SERVER, {bee_dump, Key}).
bee_dump()    -> gen_server:call(?SERVER, {bee_dump}).

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
  
  application:start(os_mon),
  
  % Opts = [named_table, ordered_set],
  % ets:new(bee_stat_total, Opts),
  
  State = #state{
    bee_stats = dict:new() 
  },
  
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
handle_call({bee_dump, Name}, _From, #state{bee_stats = Dict} = State) ->
  Dict1 = dict:filter(fun(N,_) -> N =:= Name end, Dict),
  {reply, dict:to_list(Dict1), State};
  
handle_call({bee_dump}, _From, #state{bee_stats = Dict} = State) ->
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
handle_cast({bee_stat, request_begin, Key}, #state{bee_stats = Dict} = State) ->
  {#bee_stat{current = CurrCount} = NewBackendStat, ADict} = dict_with_bee_stat(Key, Dict),
  NewBackendStat2 = NewBackendStat#bee_stat{current = CurrCount + 1},
  NewDict = dict:store(Key, NewBackendStat2, ADict),
  {noreply, State#state{bee_stats = NewDict}};
    
handle_cast({bee_stat, request_complete, Key}, #state{bee_stats = Dict} = State) ->
  {#bee_stat{total_requests = TotReq, current = CurrCount} = NewBackendStat, ADict} = dict_with_bee_stat(Key, Dict),
  
  NewBackendStat2 = NewBackendStat#bee_stat{
    current = CurrCount - 1,
    total_requests = TotReq + 1
  },
  NewDict = dict:store(Key, NewBackendStat2, ADict),
  {noreply, State#state{bee_stats = NewDict}};
  
handle_cast({bee_stat, elapsed_time, Key, Time}, #state{bee_stats = Dict} = State) ->
  {#bee_stat{total_time = TTime, average_req_time = AvgTime} = BackendStat, ADict} = dict_with_bee_stat(Key, Dict),
  Avg1 = (AvgTime + Time) / 2,
  Total1 = TTime + Time,
  NewBackendStat = BackendStat#bee_stat{average_req_time = Avg1, total_time = Total1},
  NewDict = dict:store(Key, NewBackendStat, ADict),
  {noreply, State#state{bee_stats = NewDict}};
  
handle_cast({bee_stat, socket, Key, SocketVals}, #state{bee_stats = Dict} = State) ->
  {#bee_stat{packet_count = CurrentPacketCount, bytes_received = BytesReceived} = BackendStat, 
    ADict} = dict_with_bee_stat(Key, Dict),
  
  NewBackendStat1 = case proplists:get_value(send_cnt, SocketVals) of
    undefined -> BackendStat;
    Count -> BackendStat#bee_stat{packet_count = CurrentPacketCount + Count}
  end,
  
  NewBackendStat = case proplists:get_value(send_oct, SocketVals) of
    undefined -> NewBackendStat1;
    Bytes -> NewBackendStat1#bee_stat{bytes_received = BytesReceived + Bytes}
  end,
  
  NewDict = dict:store(Key, NewBackendStat, ADict),
  {noreply, State#state{bee_stats = NewDict}};

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

new_bee_stat() ->
  #bee_stat{
    total_requests    = 0,
    current           = 0,
    total_time        = 0,
    average_req_time  = 0,
    packet_count      = 0,
    bytes_received    = 0
  }.
  
dict_with_bee_stat(Key, Dict) ->
  case dict:find(Key, Dict) of
    {ok, Val} -> {Val, Dict};
    _ -> 
      NewBackendStat = new_bee_stat(),
      {NewBackendStat, dict:store(Key, NewBackendStat, Dict)}
  end.
