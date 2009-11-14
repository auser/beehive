%%%-------------------------------------------------------------------
%%% File    : node_manager.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct  7 22:37:21 PDT 2009
%%%-------------------------------------------------------------------

-module (node_manager).

-include ("router.hrl").
-include ("common.hrl").
-include_lib("kernel/include/inet.hrl").

-behaviour(gen_server).

%% External exports
-export([
  start_link/0,
  list_nodes/0,
  stop/0
]).

-record (node, {
  hostname
}).
-record (state, {
  host      % local host
}).

-define (SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->  
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, {stop}).

list_nodes() ->
  gen_server:call(?SERVER, {list_nodes}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),  
  LocalHost = host:myip(),
  
  Opts = [named_table, set],
  ets:new(nodes, Opts),

  io:format("Nodes: ~p~n", [nodes()]),
  lists:map(fun(N) ->
    add_node(N)
  end, nodes()),
  
  {ok, #state{
    host = LocalHost
  }}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({list_nodes}, _From, State) ->
  Reply = ets:match(nodes, '$1'),
  {reply, Reply, State};
handle_call(_Call, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({stop}, State) ->
  {stop, normal, State};
handle_cast(Msg, State) ->
  error_logger:format("~s:handle_cast: got ~w\n", [?MODULE, Msg]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, State};
handle_info(Info, State) ->
  error_logger:format("~s:handle_info: got ~w\n", [?MODULE, Info]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process proxy_state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
add_node(Hostname) ->
  Node = #node{hostname = Hostname},
  ets:insert(nodes, {Hostname, Node}).