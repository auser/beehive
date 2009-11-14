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
  add_node/1,
  stop/0,
  get_host/0,
  available_hosts/0
]).

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

add_node(Host) ->
  gen_server:call(?SERVER, {add_node, Host}).

list_nodes() ->
  gen_server:call(?SERVER, {list_nodes}).
  
get_host() ->
  gen_server:call(?SERVER, {get_host}).
  
available_hosts() ->
  gen_server:call(?SERVER, {available_hosts}).

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

  % Add all nodes this router knows about
  lists:map(fun(N) -> add_node_to_node_list(N) end, nodes()),
  
  timer:send_interval(timer:seconds(20), {check_for_new_known_nodes}),
  timer:send_interval(timer:minutes(1), {update_node_pings}),
  
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
handle_call({get_host}, _From, #state{host = Host} = State) ->
  {reply, Host, State};
handle_call({available_hosts}, _From, State) ->
  {reply, get_available_hosts(), State};
handle_call({add_node, Host}, _From, State) ->
  {reply, add_node_to_node_list(Host), State};
handle_call({list_nodes}, _From, State) ->
  Reply = all_nodes(),
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
handle_info({update_node_pings}, State) ->
  update_node_pings(),
  {noreply, State};
handle_info({check_for_new_known_nodes}, State) ->
  check_for_new_known_nodes(),
  {noreply, State};
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
% Add a node to the node list. 
add_node_to_node_list(Name) ->
  case timer:tc(net_adm, ping, [Name]) of
    {Time, pong} ->
      Host = rpc:call(Name, ?MODULE, get_host, []),
      Node = #node{name = Name, host = Host, ping_distance = Time},
      ets:insert(nodes, {Name, Node});
    {_, pang} ->
      error
  end.

% Get all the nodes
all_nodes() ->
  ets:match(nodes, '$1').

% Checking for new node pings
check_for_new_known_nodes() ->
  CurrentNodeNames = lists:map(fun([{Name, _Node}]) -> Name end, all_nodes()),
  NewNodes = lists:filter(fun(N) -> lists:member(N, CurrentNodeNames) =:= false end, nodes()),
  lists:map(fun(N) -> add_node_to_node_list(N) end, NewNodes).

% Update node pings
update_node_pings() ->
  AllNodes = ets:match(nodes, '$1'),
  lists:map(fun([{Key, Node}]) ->
    case timer:tc(net_adm, ping, [Node#node.name]) of
      {Time, pong} -> ets:insert(nodes, {Key, Node#node{ping_distance = Time}});
      {_Time, pang} -> ets:delete(nodes, Key)
    end
  end, AllNodes).

% RPC call to everyone to find their host
get_available_hosts() ->
  lists:map(fun([{_N, #node{name = Name} = _Node}]) ->
    rpc:call(Name, ?MODULE, get_host, [])
  end, all_nodes()).