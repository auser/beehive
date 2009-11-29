%%%-------------------------------------------------------------------
%%% File    : bee_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct  7 22:37:21 PDT 2009
%%%-------------------------------------------------------------------

-module (bee_srv).

-include ("beehive.hrl").
-include ("common.hrl").
-include_lib("kernel/include/inet.hrl").

-behaviour(gen_server).

%% External exports
-export([
  start_link/0,
  start_link/1, 
  start_link/3
]).
-export([ get_bee/2, 
          get_proxy_state/0,
          get_host/1,
          reset_host/1, 
          reset_host/2, 
          reset_all/0,
          update_bee_status/2,
          add_bee/1,
          del_bee/1,
          maybe_handle_next_waiting_client/1
        ]). 

-define (BEEHIVE_APPS, ["beehive"]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
  LocalPort   = apps:search_for_application_value(client_port, 8080, local_port),
  ConnTimeout = apps:search_for_application_value(client_port, 120*1000, local_port),
  ActTimeout  = apps:search_for_application_value(client_port, 120*1000, local_port),
  
  start_link(LocalPort, ConnTimeout, ActTimeout).
  
%% start_link/1 used by supervisor
start_link([LocalPort, ConnTimeout, ActTimeout]) ->
  start_link(LocalPort, ConnTimeout, ActTimeout).

%% start_link/3 used by everyone else
start_link(LocalPort, ConnTimeout, ActTimeout) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalPort, ConnTimeout, ActTimeout], []).

%% Choose an available back-end host
get_bee(Pid, Hostname) ->
  gen_server:call(?MODULE, {Pid, get_bee, Hostname}, infinity).

%% Get the overall status summary of the balancer
get_proxy_state() ->
  gen_server:call(?MODULE, {get_proxy_state}).

%% Get the status summary for a back-end host.
get_host(Host) ->
  gen_server:call(?MODULE, {get_host, Host}).

update_bee_status(Backend, Status) ->
  gen_server:cast(?MODULE, {update_bee_status, Backend, Status}).

%% Reset a back-end host's status to 'ready'
reset_host(Hostname) ->
  gen_server:call(?MODULE, {reset_host, Hostname}).

%% Reset a back-end host's status to Status
%% Status = up|down
reset_host(Hostname, Status) ->
  gen_server:call(?MODULE, {reset_host, Hostname, Status}).

%% Reset all back-end hosts' status to 'up'
reset_all() ->
  gen_server:call(?MODULE, {reset_all}).

add_bee(NewBE) when is_record(NewBE, bee) ->
  gen_server:call(?MODULE, {add_bee, NewBE});

% Add a bee by name, host and port
add_bee({Name, Host, Port}) ->
  add_bee(#bee{id = {Name, Host, Port}, app_name = Name, host = Host, port = Port}).

%% Delete a back-end host from the balancer's list.
del_bee(Host) ->
  gen_server:call(?MODULE, {del_bee, Host}).
  
maybe_handle_next_waiting_client(Name) ->
  gen_server:cast(?MODULE, {maybe_handle_next_waiting_client, Name}).

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
init([LocalPort, ConnTimeout, ActTimeout]) ->
  process_flag(trap_exit, true),
  ?NOTIFY({?MODULE, init}),
  
  Pid     = whereis(tcp_socket_server),
  
  LocalHost = host:myip(),
  
  add_bees_from_config(),

  % {ok, TOTimer} = timer:send_interval(1000, {check_waiter_timeouts}),
  {ok, #proxy_state{
    local_port = LocalPort, 
    local_host = LocalHost,
    conn_timeout = ConnTimeout,
    act_timeout = ActTimeout,
    start_time = date_util:now_to_seconds(), 
    % to_timer = TOTimer, 
    acceptor = Pid
    }
  }.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({Pid, get_bee, Hostname}, From, State) ->
  % If this is a request for an internal application, then serve that first
  % These are abnormal applications because they MUST be running for every router
  % and bee_srv. 
  case Hostname of
    base ->
      Port = apps:search_for_application_value(beehive_app_port, 4999, beehive), 
      Host = {127,0,0,1},
      Id = {Hostname, Host, Port},
      
      Backend = #bee{ 
        id = Id, port = Port, host = Host, app_name = Hostname
      },
      {reply, {ok, Backend}, State};
    _ ->
      MetaParam = case app:find_by_name(Hostname) of
        [] -> apps:search_for_application_value(bee_strategy, random, beehive);
        App -> App#app.routing_param
      end,
      case choose_bee({MetaParam, Hostname}, From, Pid) of
    	  ?MUST_WAIT_MSG -> 
    	    timer:apply_after(3000, ?MODULE, maybe_handle_next_waiting_client, [Hostname]),
    	    {noreply, State};
    	  {ok, Backend} -> 
    	    {reply, {ok, Backend}, State};
    	  {error, Reason} -> {reply, {error, Reason}, State};
    	  E ->
    	    ?LOG(error, "Got weird response in get_bee: ~p", [E]),
    	    {noreply, State}
      end
  end;
handle_call({get_proxy_state}, _From, State) ->
  Reply = State,
  {reply, Reply, State};
handle_call({get_host, Hostname}, _From, State) ->
  Reply = apps:lookup(bees, Hostname),
  {reply, Reply, State};
handle_call({add_bee, NewBE}, _From, State) ->
  {reply, handle_add_bee(NewBE), State};
handle_call(Request, From, State) ->
  error_logger:format("~s:handle_call: got ~w from ~w\n", [?MODULE, Request, From]),
  Reply = error,
  {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({maybe_handle_next_waiting_client, Hostname}, State) ->
  maybe_handle_next_waiting_client(Hostname, State),
  {noreply, State};
  
handle_cast(Msg, State) ->
  error_logger:format("~s:handle_cast: got ~w\n", [?MODULE, Msg]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, shutdown}, State) when Pid == State#proxy_state.acceptor ->
  ?LOG(error, "~s:handle_info: acceptor pid ~w shutdown\n", [?MODULE, Pid]),
  {stop, normal, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  case State#proxy_state.acceptor of
	  Pid ->
	    %% Acceptor died but not because of shutdown request.
	    ?LOG(info, "~s:handle_info: acceptor pid ~w died, reason = ~w\n", [?MODULE, Pid, Reason]),
	    {stop, {acceptor_failed, Pid, Reason}, State};
	  _ ->
      {noreply, State}
  end;
handle_info(Info, State) ->
    error_logger:format("~s:handle_info: got ~w\n", [?MODULE, Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
  timer:cancel(State#proxy_state.to_timer),
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
choose_bee({_, Name} = Tuple, From, FromPid) ->
  case choose_bee(Tuple) of
	  {ok, Backend} -> {ok, Backend};
	  {error, Reason} -> {error, Reason};
	  ?MUST_WAIT_MSG ->
	    ?QSTORE:push(?WAIT_DB, Name, {Tuple, From, FromPid, date_util:now_to_seconds()}),
      ?MUST_WAIT_MSG
  end.

% Find a bee host that is ready and choose it based on the strategy
% given for the bee
% Tuple = {name, Hostname}
choose_bee({RoutingParameter, Hostname}) ->
  case bee:find_all_by_name(Hostname) of
    [] -> 
      case app:exist(Hostname) of
        false ->
          {error, unknown_app};
        true ->
          ?NOTIFY({app, request_to_start_new_bee, Hostname}),
          ?MUST_WAIT_MSG
      end;
    Backends ->
      % We should move this out of here so that it doesn't slow down the proxy
      % as it is right now, this will slow down the proxy quite a bit
      AvailableBackends = lists:filter(fun(B) -> (catch B#bee.status =:= ready) end, Backends),
      case choose_from_bees(AvailableBackends, RoutingParameter) of
        ?MUST_WAIT_MSG ->
          % This might not be appropriate... not sure yet
          ?NOTIFY({app, request_to_start_new_bee, Hostname}),
          ?MUST_WAIT_MSG;
        E -> E
      end
  end.

% Choose from the list of bees
% Here is the logic to choose a bee from the list of bees
% The user defines the preferred strategy for choosing backends when starting
% the router with the -g option
% i.e. start_beehive.com -g random
% If the application defines it's own routing mechanism with the routing_param
% then that is used to choose the backend, otherwise the default router param will
% be used
choose_from_bees([], _RoutingParameter) -> ?MUST_WAIT_MSG;
choose_from_bees(Backends, AppRoutingParam) ->
  PreferredStrategy = apps:search_for_application_value(bee_strategy, random, beehive),
  Fun = case AppRoutingParam of
    undefined -> PreferredStrategy;
    F -> F
  end,
  Backend = bee_strategies:Fun(Backends),
  {ok, Backend}.

% Handle adding a new bee
handle_add_bee(NewBE) ->
  bee:create(NewBE).

% Handle the *next* pending client only. 
% Perhaps this should go somewhere else in the stack, but for the time being
% we'll leave it in here for relativity purposes
maybe_handle_next_waiting_client(Name, State) ->
  TOTime = date_util:now_to_seconds() - (State#proxy_state.conn_timeout / 1000),
  case ?QSTORE:pop(?WAIT_DB, Name) of
    empty -> ok;
    % If the request was made at conn_timeout seconds ago
    {value, {_Hostname, From, _Pid, InsertTime}} when InsertTime < TOTime ->
      gen_server:reply(From, ?BACKEND_TIMEOUT_MSG),
      maybe_handle_next_waiting_client(Name, State);
    {value, {{_RoutingParam, Hostname} = Tuple, From, Pid, _InsertTime}} ->
      ?LOG(info, "Handling Q: ~p", [Hostname]),
      case choose_bee(Tuple, From, Pid) of
        % Clearly we are not ready for another bee connection request. :(
        % choose_bee puts the request in the pending queue, so we don't have
        % to take care of that here
        ?MUST_WAIT_MSG -> ok;
        {ok, B} -> gen_server:reply(From, {ok, B})
      end
  end.
  
% Basic configuration stuff
% Add apps from a configuration file
add_bees_from_config() ->
  case apps:search_for_application_value(bees, undefined, beehive) of
    undefined -> ok;
    RawPath ->
      case (catch file_utils:abs_or_relative_filepath(RawPath)) of
        {error, _} -> "router.log";
        Path -> 
          case file:consult(Path) of
            {ok, List} ->
              F = fun(V) ->
                case V of
                  {Name, Host, Port} ->
                    ?LOG(info, "Adding app: ~p, ~p:~p", [Name, Host, Port]),
                    bee:create(#bee{id={Name, Host, Port}, app_name = Name, host = Host, port = Port, status = ready})
                end
              end,
              lists:map(F, List);
            _E -> 
              ok
          end
      end
  end.
