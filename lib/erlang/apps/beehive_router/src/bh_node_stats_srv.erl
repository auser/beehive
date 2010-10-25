%%%-------------------------------------------------------------------
%%% File    : bh_node_stats_srv.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Sun Dec 13 20:51:11 PST 2009
%%%-------------------------------------------------------------------

-module (bh_node_stats_srv).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  node_stat/1,
  node_dump/0,
  node_dump/1,
  node_dump/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  node_stats   % dict of the node stats
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
node_stat({node_stat, Key, Value, Time})        ->
  gen_server:cast(?SERVER, {node_stat, Key, Value, Time}).

node_dump() -> gen_server:call(?SERVER, {node_dump}).
node_dump(Key) -> gen_server:call(?SERVER, {node_dump, Key}).
node_dump(Key, Range) -> gen_server:call(?SERVER, {node_dump, Key, Range}).

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
  io:format("Starting bh_node_stats_srv~n"),
  process_flag(trap_exit, true),

  application:start(os_mon),

  State = #state{
    node_stats  = dict:new()
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
handle_call({node_dump}, _From, #state{node_stats = Dict} = State) ->
  {reply, dict:to_list(Dict), State};

handle_call({node_dump, Key}, _From, #state{node_stats = Dict} = State) ->
  Reply = case dict:find(Key, Dict) of
    error -> [];
    {ok, E} -> E
  end,
  {reply, Reply, State};

handle_call({node_dump, Key, Range},
            _From, #state{node_stats = Dict} = State) ->
  StatsList = case dict:find(Key, Dict) of
    error -> [];
    {ok, E} -> E
  end,
  Reply = lists:sublist(StatsList, Range),
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
handle_cast({node_stat, Key, Value, Time}, #state{node_stats = Dict} = State) ->
  NewDict = case dict:find(Key, Dict) of
    error -> dict:store(Key, [{Time, Value}], Dict);
    {ok, CurrentVal} -> dict:store(Key, [{Time, Value}|CurrentVal], Dict)
  end,
  {noreply, State#state{node_stats = NewDict}};

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
