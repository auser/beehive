%%%-------------------------------------------------------------------
%%% File    : queue_store.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct 28 16:03:12 PDT 2009
%%%-------------------------------------------------------------------

-module (queue_store).
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  pop/2,
  push/3,
  replace/3,
  get_queue/2,
  delete_queue/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define (DICT, dict).

-record (state, {
  queues      % dict of queues
}).

%%====================================================================
%% API
%%====================================================================
pop (Name, QName) -> gen_server:call(Name, {next, QName}).
push(Name, QName, Item) -> gen_server:call(Name, {push, QName, Item}).
replace(Name, QName, NewQ) -> gen_server:call(Name, {replace, QName, NewQ}).
get_queue(Name, QName) -> gen_server:call(Name, {get_queue, QName}).
delete_queue(Name, QName) -> gen_server:call(Name, {delete_queue, QName}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

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
  {ok, #state{queues = ?DICT:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({next, Name}, _From, #state{queues = QDict} = State) ->
  case fetch_q(Name, QDict) of
    empty -> {reply, empty, State};
    Q ->
      {O, NewQ} = queue:out(Q),
      {reply, O, store_q(Name, NewQ, State)}
  end;
handle_call({push, Name, Item}, _From, #state{queues = QDict} = State) ->
  NewState = case fetch_q(Name, QDict) of
    empty -> 
      NewQ = queue:new(),
      NamedQ = queue:in(Item, NewQ),
      store_q(Name, NamedQ, State);
    Q ->
      store_q(Name, queue:in(Item, Q), State)
  end,
  {reply, pushed, NewState};
  
handle_call({replace, Name, NewQ}, _From, State) ->
  {reply, replaced, store_q(Name, NewQ, State)};
handle_call({get_queue, Name}, _From, #state{queues = QDict} = State) ->
  case fetch_q(Name, QDict) of
    empty -> {reply, empty, State};
    Q ->
      {reply, Q, State}
  end;
handle_call({delete_queue, Name}, _From, State) ->
  {reply, delete_q(Name, State), State};
  
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
fetch_q(Name, QDict) ->
  case dict:find(Name, QDict) of
    error -> empty;
    {ok, V} -> V
  end.

store_q(Name, NewQ, #state{queues = QDict} = State) ->
  NewDict = ?DICT:store(Name, NewQ, QDict),
  State#state{queues = NewDict}.

delete_q(Name, #state{queues = QDict} = State) ->
  NewDict = ?DICT:erase(Name, QDict),
  State#state{queues = NewDict}.