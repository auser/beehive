%%%-------------------------------------------------------------------
%%% File    : dict_store.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct 21 12:10:30 PDT 2009
%%%-------------------------------------------------------------------

-module (dict_store).
-include ("common.hrl").
-behaviour(gen_server).

%% API
-export([ 
          start_link/1,
          stop/1,
          store/3,
          lookup/2,
          delete/2,
          filter/2,
          all/1,
          map/2,
          test/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
store(Name, Key, Val) -> gen_server:call(Name, {store, Key, Val}).
lookup(Name, Key) -> gen_server:call(Name, {lookup, Key}).
delete(Name, Key) -> gen_server:call(Name, {delete, Key}).
stop(Name)  -> gen_server:cast(Name, stop).
filter(Name, Fun) -> gen_server:call(Name, {filter, Fun}).
all(Name) -> gen_server:call(Name, {all}).
map(Name, Fun) -> gen_server:call(Name, {map, Fun}).

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
  {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({store, Key, Val}, _From, Dict) ->
  Dict1 = dict:store(Key, Val, Dict),
  {reply, ack, Dict1};
  
handle_call({lookup, Key}, _From, Dict) ->
  case dict:find(Key, Dict) of
    {ok, Val} -> {reply, Val, Dict};
    Else -> {reply, Else, Dict}
  end;

handle_call({delete, Key}, _From, Dict) ->
  Dict1 = dict:erase(Key, Dict),
  {reply, ack, Dict1};

handle_call({all}, _From, Dict) ->
  Dict1 = dict:filter(fun(_,_) -> true end, Dict),
  {reply, dict:to_list(Dict1), Dict1};
  
handle_call({filter, Fun}, _From, Dict) ->
  Dict1 = dict:filter(Fun, Dict),
  {reply, dict:to_list(Dict1), Dict1};
  
handle_call({map, Fun}, _From, Dict) ->
  Dict1 = dict:map(Fun, Dict),
  {reply, dict:to_list(Dict1), Dict1};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, Dict) -> {stop, normal, Dict};
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
terminate(Reason, _State) ->
  ?LOG(info, "Terminating ~p because ~p", [?MODULE, Reason]),
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

test() ->
    start_link(test),
    store(test, a, "a"),
    store(test, b, "b"),
    store(test, "c", {a,b,c}),
    io:format("~p, ~p, ~p, ~p, ~p, ~p~n", [
      lookup(test, a) =:= "a",
      lookup(test, b) =:= "b",
      lookup(test, "c") =:= {a,b,c},
      lookup(test, c) =:= error,
      delete(test, a) =:= ack,
      lookup(test, a) =:= error
    ]),
    stop(test).
  