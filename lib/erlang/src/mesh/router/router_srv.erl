%%%-------------------------------------------------------------------
%%% File    : router_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct  7 22:37:21 PDT 2009
%%%-------------------------------------------------------------------

-module (router_srv).

-include ("beehive.hrl").
-include ("common.hrl").
-include_lib("kernel/include/inet.hrl").

-behaviour(gen_cluster).

%% External exports
-export([
  start_link/0,
  start_link/1,
  seed_nodes/1
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

% gen_cluster callback
-export([handle_join/3, handle_leave/4]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
seed_nodes(_State) -> global:whereis_name(node_manager).

start_link() ->
  LocalPort   = config:search_for_application_value(client_port, 8080,     router),
  ConnTimeout = config:search_for_application_value(connection_timeout, 120*1000, router),
  ActTimeout  = config:search_for_application_value(activity_timeout, 120*1000, router),
  
  Args = [{local_port, LocalPort}, {connection_timeout, ConnTimeout}, {activity_timeout, ActTimeout}],
  start_link(Args).
  
%% start_link/3 used by everyone else
start_link(Args) ->
  gen_cluster:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% Choose an available back-end host
get_bee(Pid, Hostname) ->
  gen_cluster:call(?MODULE, {Pid, get_bee, Hostname}, infinity).

%% Get the overall status summary of the balancer
get_proxy_state() ->
  gen_cluster:call(?MODULE, {get_proxy_state}).

%% Get the status summary for a back-end host.
get_host(Host) ->
  gen_cluster:call(?MODULE, {get_host, Host}).

update_bee_status(Backend, Status) ->
  gen_cluster:cast(?MODULE, {update_bee_status, Backend, Status}).

%% Reset a back-end host's status to 'ready'
reset_host(Hostname) ->
  gen_cluster:call(?MODULE, {reset_host, Hostname}).

%% Reset a back-end host's status to Status
%% Status = up|down
reset_host(Hostname, Status) ->
  gen_cluster:call(?MODULE, {reset_host, Hostname, Status}).

%% Reset all back-end hosts' status to 'up'
reset_all() ->
  gen_cluster:call(?MODULE, {reset_all}).

add_bee(NewBE) when is_record(NewBE, bee) ->
  gen_cluster:call(?MODULE, {add_bee, NewBE});

% Add a bee by name, host and port
add_bee({Name, Host, Port}) ->
  add_bee(#bee{id = {Name, Host, Port}, app_name = Name, host = Host, port = Port}).

%% Delete a back-end host from the balancer's list.
del_bee(Host) ->
  gen_cluster:call(?MODULE, {del_bee, Host}).
  
maybe_handle_next_waiting_client(Name) ->
  gen_cluster:cast(?MODULE, {maybe_handle_next_waiting_client, Name}).

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
init(Args) ->
  process_flag(trap_exit, true),
  LocalPort = proplists:get_value(local_port, Args, 8080),
  ConnTimeout = proplists:get_value(connection_timeout, Args, 30),
  ActTimeout = proplists:get_value(activity_timeout, Args, 10),
  
  Pid     = whereis(tcp_socket_server),
  
  LocalHost = bh_host:myip(),
  
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
  % and router_srv. 
  case Hostname of
    base ->
      Port = config:search_for_application_value(beehive_app_port, 4999, router), 
      Host = {127,0,0,1},
      Id = {Hostname, Host, Port},
      
      Backend = #bee{ 
        id = Id, port = Port, host = Host, app_name = Hostname
      },
      {reply, {ok, Backend}, State};
    _ ->
      {AppMod, MetaParam} = case apps:find_by_name(Hostname) of
        [] ->
          M = config:search_for_application_value(bee_picker, bee_strategies, router),
          Meta = config:search_for_application_value(bee_strategy, random, router),
          {M, Meta};
        App ->
          pick_mod_and_meta_from_app(App)
      end,
      case choose_bee({Hostname, AppMod, MetaParam}, From, Pid) of
    	  ?MUST_WAIT_MSG -> 
    	    timer:apply_after(1000, ?MODULE, maybe_handle_next_waiting_client, [Hostname]),
    	    {noreply, State};
    	  {ok, Backend} -> 
    	    {reply, {ok, Backend}, State};
    	  {error, Reason} -> 
    	    ?LOG(error, "handle_call (~p:~p) failed because: ~p, ~p", [?MODULE, ?LINE, Reason, Hostname]),
    	    {reply, {error, Reason}, State};
    	  Else ->
    	    ?LOG(error, "Got weird response in get_bees: ~p", [Else]),
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

handle_cast(stop, State) -> 
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
handle_info(_Info, State) ->
  % error_logger:format("~s:handle_info: got ~w\n", [?MODULE, Info]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
  % timer:cancel(State#proxy_state.to_timer),
  ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process proxy_state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(_JoiningPid, _Pidlist, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_leave(LeavingPid, Pidlist, Info, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------
handle_leave(_LeavingPid, _Pidlist, _Info, State) ->
  {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
choose_bee({Name, _, _} = Tuple, From, FromPid) ->
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
choose_bee({Hostname, AppMod, RoutingParameter}) ->
  case (catch bees:find_all_by_name(Hostname)) of
    [] -> 
      case apps:exist(Hostname) of
        false ->
          {error, unknown_app};
        true ->
          % {app, request_to_start_new_bee, App, Host, Sha}
          ?NOTIFY({app, request_to_start_new_bee, Hostname}),
          ?MUST_WAIT_MSG
      end;
    {'EXIT', {_, {no_exists, Err}}} ->
      ?LOG(error, "No exists for bees: ~p", [Err]),
      ?NOTIFY({db, database_not_initialized, bees}),
      ?MUST_WAIT_MSG;
    {'EXIT', _} ->
      ?LOG(error, "Undefined error with choose_bee", []),
      ?MUST_WAIT_MSG;
    Backends ->
      % We should move this out of here so that it doesn't slow down the proxy
      % as it is right now, this will slow down the proxy quite a bit
      AvailableBackends = lists:filter(fun(B) -> (catch B#bee.status =:= ready) end, Backends),
      case choose_from_bees(AvailableBackends, AppMod, RoutingParameter) of
        ?MUST_WAIT_MSG ->
          case apps:exist(Hostname) of
            false ->
              {error, unknown_app};
            true ->
              ?NOTIFY({app, request_to_start_new_bee, Hostname}),
              ?MUST_WAIT_MSG
          end;
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
choose_from_bees([], _AppMod, _RoutingParameter) -> ?MUST_WAIT_MSG;
choose_from_bees(Backends, Mod, AppRoutingParam) ->
  PreferredStrategy = config:search_for_application_value(bee_strategy, random, router),
  Fun = case AppRoutingParam of
    undefined -> PreferredStrategy;
    F -> F
  end,
  Backend = Mod:Fun(Backends),
  {ok, Backend}.

% Handle adding a new bee
handle_add_bee(NewBE) ->
  bees:create(NewBE).

% Handle the *next* pending client only. 
% Perhaps this should go somewhere else in the stack, but for the time being
% we'll leave it in here for relativity purposes
maybe_handle_next_waiting_client(Name, State) ->
  TOTime = date_util:now_to_seconds() - (State#proxy_state.conn_timeout / 1000),
  case ?QSTORE:pop(?WAIT_DB, Name) of
    empty -> ok;
    % If the request was made at conn_timeout seconds ago
    {value, {_Hostname, From, _Pid, InsertTime}} when InsertTime > TOTime ->
      ?LOG(info, "Still not a timeout: ~p > ~p", [InsertTime, TOTime]),
      gen_cluster:reply(From, ?BACKEND_TIMEOUT_MSG),
      maybe_handle_next_waiting_client(Name, State);
    {value, {{Hostname, _AppMod, _RoutingParam} = Tuple, From, Pid, _InsertTime}} ->
      ?LOG(info, "Handling Q: ~p (~p)", [Hostname, Tuple]),
      case choose_bee(Tuple, From, Pid) of
        % Clearly we are not ready for another bee connection request. :(
        % choose_bee puts the request in the pending queue, so we don't have
        % to take care of that here
        ?MUST_WAIT_MSG -> ok;
        {ok, B} -> gen_cluster:reply(From, {ok, B})
      end
  end.
  
% Basic configuration stuff
% Add apps from a configuration file
add_bees_from_config() ->
  case config:search_for_application_value(bees, undefined, router) of
    undefined -> ok;
    RawPath ->
      case (catch bh_file_utils:abs_or_relative_filepath(RawPath)) of
        {error, _} -> "router.log";
        Path -> 
          case file:consult(Path) of
            {ok, List} ->
              F = fun(V) ->
                case V of
                  {Name, Host, Port} ->
                    ?LOG(info, "Adding app: ~p, ~p:~p", [Name, Host, Port]),
                    bees:create(#bee{id={Name, Host, Port}, app_name = Name, host = Host, port = Port, status = ready})
                end
              end,
              lists:map(F, List);
            _E -> 
              ok
          end
      end
  end.

% These are commands that can be specified on an application
% bee_picker is the custom module to choose the bee from
% routing_param is the name of the method in the bee_picker
% Defaults to bee_strategies:random if none are specified on the app
pick_mod_and_meta_from_app(App) ->
  Mod = case App#app.bee_picker of
    undefined -> config:search_for_application_value(bee_picker, bee_strategies, router);
    E -> E
  end,
  R = case App#app.routing_param of
    undefined -> config:search_for_application_value(bee_strategy, random, router);
    El -> El
  end,
  {Mod, R}.