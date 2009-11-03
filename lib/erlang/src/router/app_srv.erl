%%%-------------------------------------------------------------------
%%% File    : app_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Oct  7 22:37:21 PDT 2009
%%%-------------------------------------------------------------------

-module (app_srv).

-include ("router.hrl").
-include ("common.hrl").
-include_lib("kernel/include/inet.hrl").

-behaviour(gen_server).

%% External exports
-export([
  start_link/0,
  start_link/1, 
  start_link/3
]).
-export([ get_backend/1, 
          remote_ok/2, 
          remote_error/2,
          remote_done/2,
          get_proxy_state/0,
          get_host/1,
          reset_host/1, 
          reset_host/2, 
          reset_all/0,
          update_backend_status/2,
          add_backend/1,
          del_backend/1
        ]). 

%% Debugging exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
  LocalPort   = apps:search_for_application_value(client_port, 8080, local_port),
  ConnTimeout = apps:search_for_application_value(client_port, 1*1000, local_port),
  ActTimeout  = apps:search_for_application_value(client_port, 120*1000, local_port),
  
  start_link(LocalPort, ConnTimeout, ActTimeout).
  
%% start_link/1 used by supervisor
start_link([LocalPort, ConnTimeout, ActTimeout]) ->
  start_link(LocalPort, ConnTimeout, ActTimeout).

%% start_link/3 used by everyone else
start_link(LocalPort, ConnTimeout, ActTimeout) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalPort, ConnTimeout, ActTimeout], []).

%% Choose an available back-end host
get_backend(Hostname) ->
  gen_server:call(?MODULE, {self(), get_backend, Hostname}).

%% Tell the balancer that our assigned back-end is OK.
%% Note that we don't pass the hostname back to the balancer.  That's
%% because the balancer only needs our PID, self(), to figure
%% everything else out.
remote_ok(Backend, From) ->
  gen_server:cast(?MODULE, {From, remote_ok, Backend}).

%% Tell the balancer that our assigned back-end cannot be used.
remote_error(Backend, Error) ->
  gen_server:cast(?MODULE, {self(), remote_error, Backend, Error}).

% Free one backend
remote_done(Backend, From) ->
  gen_server:cast(?MODULE, {From, remote_done, Backend}).

%% Get the overall status summary of the balancer
get_proxy_state() ->
  gen_server:call(?MODULE, {get_proxy_state}).

%% Get the status summary for a back-end host.
get_host(Host) ->
  gen_server:call(?MODULE, {get_host, Host}).

update_backend_status(Backend, Status) ->
  gen_server:cast(?MODULE, {update_backend_status, Backend, Status}).

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

add_backend(NewBE) when is_record(NewBE, backend) ->
  gen_server:call(?MODULE, {add_backend, NewBE});

% Add a backend by name, host and port
add_backend({Name, Host, Port}) ->
  add_backend(#backend{name = Name, host = Host, port = Port});

% Add a backend by proplists
add_backend(Proplist) ->
  NewBackend = create_backend_from_proplist(#backend{}, Proplist),
  ?LOG(info, "Trying to add backend with proplists: ~p", [NewBackend]),
  add_backend(NewBackend).

%% Delete a back-end host from the balancer's list.
del_backend(Host) ->
  gen_server:call(?MODULE, {del_backend, Host}).

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
  Pid     = whereis(http_client_srv),
  
  LocalHost = host:myip(),
  
  ?LOG(info, "Started ~p:start_link(~p)", [?KVSTORE, ?BACKEND_DB]),
  ?KVSTORE:start_link(?BACKEND_DB),
  ?QSTORE:start_link(?WAIT_DB),

  add_backends_from_config(),

  {ok, TOTimer} = timer:send_interval(1000, {check_waiter_timeouts}),
  {ok, #proxy_state{
    local_port = LocalPort, 
    local_host = LocalHost,
    conn_timeout = ConnTimeout,
    act_timeout = ActTimeout,
    start_time = date_util:now_to_seconds(), 
    to_timer = TOTimer, 
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
handle_call({Pid, get_backend, Hostname}, From, State) ->
  case choose_backend(Hostname, From, Pid) of
	  ?MUST_WAIT_MSG -> {noreply, State};
	  {ok, Backend} -> {reply, {ok, Backend}, State};
	  {error, Reason} -> {reply, {error, Reason}, State};
	  E ->
	    ?LOG(error, "Got weird response in get_backend: ~p", [E]),
	    {noreply, State}
  end;
handle_call({get_proxy_state}, _From, State) ->
  Reply = State,
  {reply, Reply, State};
handle_call({get_host, Hostname}, _From, State) ->
  Reply = apps:lookup(backends, Hostname),
  {reply, Reply, State};
handle_call({reset_host, Hostname}, _From, State) ->
  {reply, reset_backend_host(Hostname, ready), State};
handle_call({reset_host, Hostname, ready}, _From, State) ->
  Reply = reset_backend_host(Hostname, ready),
  %% This is a dirty trick.  :-) Since we know that a backend is now
  %% up and available, we'll send a process exit message to ourself.
  %% Receipt of such a message will trigger the first waiter, if
  %% any, to be assigned a backend.
  self() ! {'EXIT', no_such_pid, another_host_is_up_now},
  {reply, Reply, State};
handle_call({reset_host, Hostname, down}, _From, State) ->
  Reply = reset_backend_host(Hostname, down),
  {reply, Reply, State};
handle_call({reset_backend, Backend, Status}, _From, State) ->
  reset_backend(Backend, Status),
  {reply, ok, State};
handle_call({add_backend, NewBE}, _From, State) ->
  {reply, handle_add_backend(NewBE), State};
handle_call({del_backend, Backend}, _From, State) ->
  {reply, delete_backend(Backend), State};
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
handle_cast({Pid, remote_ok, Backend}, State) ->
  link(Pid), % So we can watch when the proxy is done
  mark_backend_busy(Pid, Backend),
  {noreply, State};
handle_cast({Pid, remote_error, Backend, Error}, State) ->
  mark_backend_broken(Pid, Error, Backend),
  {noreply, State};
handle_cast({Pid, remote_done, Backend}, State) ->
  unlink(Pid),
  NewBe = mark_backend_ready(Pid, Backend),
  spawn(fun() -> maybe_handle_next_waiting_client(NewBe, State) end),
  {noreply, State};
handle_cast({update_backend_status, Backend, Status}, State) ->
  save_backend(Backend#backend{status = Status}),
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
  unlink(Pid),
  case State#proxy_state.acceptor of
	  Pid ->
	    %% Acceptor died but not because of shutdown request.
	    ?LOG(info, "~s:handle_info: acceptor pid ~w died, reason = ~w\n", [?MODULE, Pid, Reason]),
	    {stop, {acceptor_failed, Pid, Reason}, State};
	  _ ->
      {noreply, State}
  end;
handle_info({check_waiter_timeouts}, State) ->
    check_waiter_timeouts(State),
    {noreply, State};
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

choose_backend(Hostname, From, FromPid) ->
  case choose_backend(Hostname, FromPid) of
	  {ok, Backend} -> {ok, Backend};
	  {error, Reason} -> {error, Reason};
	  ?MUST_WAIT_MSG ->
	    ?QSTORE:push(?WAIT_DB, Hostname, {Hostname, From, FromPid, date_util:now_to_seconds()}),
      ?MUST_WAIT_MSG
  end.

%% Find the first available back-end host

choose_backend(Hostname, FromPid) ->
  case apps:lookup(backends, Hostname) of
    error -> {error, unknown_app};
    [] -> {error, unknown_app};
    Backends ->
      choose_from_backends(Backends, Hostname, FromPid)
  end.

choose_from_backends([], _Hostname, _FromPid) -> ?MUST_WAIT_MSG;
choose_from_backends([Backend|Rest], Hostname, FromPid) ->
  if
    Backend#backend.status =:= ready
      andalso (length(Backend#backend.pidlist) < Backend#backend.maxconn)
      andalso Backend#backend.name =:= Hostname ->
        NewBackend = mark_backend_pending(FromPid, Backend),
        {ok, NewBackend};
    true ->
      choose_from_backends(Rest, Hostname, FromPid)
  end.

reset_backend_host(Hostname, Status) ->
  case apps:lookup(backends, Hostname) of
    error -> {error, unknown_app};
    [] -> {error, unknown_app};
    Backends -> 
      lists:map(fun(B) -> reset_backend(B, Status) end, Backends),
      {ok, reset}
  end.

reset_backend(Backend, Status) ->
  save_backend(Backend#backend{status = Status, lasterr = reset, lasterr_time = date_util:now_to_seconds()}).
  
handle_add_backend(NewBE) when is_record(NewBE, backend) ->
  case catch sane_be(NewBE) of
	  true ->
	    ?LOG(info, "Storing backend for: ~p", [NewBE#backend.name]),
	    case apps:lookup(backends, NewBE#backend.name) of
        % undefined means the host doesn't exist in the system yet, so let's add it
        undefined -> apps:store(backend, NewBE#backend.name, [NewBE]);
		    _ -> save_backend(NewBE)
	    end;
	  _ -> {error, not_sane}
  end.

% Handle the *next* pending client only. 
maybe_handle_next_waiting_client(#backend{name = Name} = Backend, State) ->
  TOTime = date_util:now_to_seconds() - (State#proxy_state.conn_timeout / 1000),
  case ?QSTORE:pop(?WAIT_DB, Name) of
    empty -> ok;
    {value, {_Hostname, From, _Pid, InsertTime}} when InsertTime < TOTime ->
      gen_server:reply(From, ?BACKEND_TIMEOUT_MSG),
      maybe_handle_next_waiting_client(Backend, State);
    {value, {Hostname, From, Pid, _InsertTime} = Item} ->
      case choose_backend(Hostname, From, Pid) of
        ?MUST_WAIT_MSG -> 
          ?QSTORE:push(?WAIT_DB, Hostname, Item),
          ok; % Clearly we are not ready for another backend connection request. :(
        {ok, B} ->
          gen_server:reply(From, {ok, B})
      end
  end.
  
% Check to make sure the backend is "sane"
sane_be(B) ->
  Real = #backend{},
  if 
	  size(B) =/= size(Real) -> false;
  	B#backend.name == "" -> false;
  	B#backend.port =< 0 -> false;
  	B#backend.maxconn =< 0 -> false;
  	B#backend.act_count =/= 0 -> false;
  	B#backend.act_time =/= 0 -> false;
  	B#backend.pidlist =/= [] -> false;
  	true -> true
  end.

check_waiter_timeouts(State) ->
  TOTime = date_util:now_to_seconds() - (State#proxy_state.conn_timeout / 1000),
  AllBackends = apps:all(backends),
  lists:map(fun(B) ->
    NewQ = handle_timeout_queue(B#backend.name, TOTime, queue:new()),
    ?QSTORE:replace(?WAIT_DB, B#backend.name, NewQ)
  end, AllBackends),
  State.

handle_timeout_queue(Name, TOTime, NewQ) ->
  case ?QSTORE:pop(?WAIT_DB, Name) of
    {value, Item} ->
      case Item of
        {_Hostname, From, _FromPid, Time} when Time < TOTime ->
          gen_server:reply(From, ?BACKEND_TIMEOUT_MSG),
          NewQ;
        _ ->
          handle_timeout_queue(Name, TOTime, queue:in(Item, NewQ))
      end;
    empty -> NewQ
  end.
  
% Basic configuration stuff
% Add apps from a configuration file
add_backends_from_config() ->
  case apps:search_for_application_value(backends, undefined, router) of
    undefined -> ok;
    RawPath -> 
      Path = case filelib:is_file(RawPath) of
        true -> RawPath;
        false -> filename:join([filename:absname(""), RawPath])
      end,
      case file:consult(Path) of
        {ok, List} ->
          F = fun(V) ->
            case V of
              {Name, Host, Port} ->
                ?LOG(info, "Adding app: ~p, ~p:~p", [Name, Host, Port]),
                save_backend(#backend{name = Name, host = Host, port = Port, status = ready})
            end
          end,
          lists:map(F, List);
        _E -> 
          ok
      end
  end.

% Mark the backend instance as pending
mark_backend_pending(Pid, #backend{pidlist = PidList} = Backend) ->
  save_backend(Backend#backend{
    status = ready,
    pidlist = [{pending, Pid, date_util:now_to_seconds()}|PidList]
  }).
% Mark this instance as busy
mark_backend_busy(Pid, #backend{pidlist = OrigPidlist, maxconn = MaxConn} = Backend)   -> 
  Status = if 
    length(OrigPidlist) > MaxConn -> busy;
    true -> ready
  end,
  Pidlist = lists:keyreplace(Pid, 2, OrigPidlist, {active, Pid, date_util:now_to_seconds()}),
  save_backend(Backend#backend{
    status = Status,
    pidlist = Pidlist
  }).
% Mark this instance as ready
mark_backend_ready(Pid, #backend{pidlist = PidList, act_time = CurrActTime, act_count = ActCount} = Backend)  -> 
  {value, {_, _, StartTime}} = lists:keysearch(Pid, 2, PidList),
  ActTime = date_util:now_to_seconds() - StartTime,
  NewBackend = Backend#backend{
    status = ready,
    act_time = CurrActTime + ActTime,
    act_count = ActCount + 1,
    pidlist = lists:keydelete(Pid, 2, PidList)
  },
  ?EVENT_MANAGER:notify({backend, ready, NewBackend}),
  save_backend(NewBackend).
% Mark an instance as broken
mark_backend_broken(Pid, ErrorStatus, #backend{pidlist = PidList} = Backend) ->
  ?LOG(error, "update_backend: Pid ~w for host ~s ~w, error status ~w\n", [Pid, Backend#backend.name, Backend#backend.port, ErrorStatus]),
  save_backend(Backend#backend{
    status = down, 
    lasterr = ErrorStatus,
    lasterr_time = date_util:now_to_seconds(),
	  pidlist = lists:keydelete(Pid, 2, PidList)
	}).

% Save the backend into the backends lookup
save_backend(#backend{name = Hostname} = Backend) ->
  NewBackend = Backend#backend{lastresp_time = date_util:now_to_seconds()},
  F = fun() ->
    CurrentBackends = apps:lookup(backends, Hostname),
    OtherBackends = delete_backend_from_list(Backend, CurrentBackends),
    apps:store(backend, Hostname, [NewBackend|OtherBackends])
  end,
  F(),
  NewBackend.

% Delete a backend from the backends lookup
delete_backend(#backend{name = Hostname} = Backend) ->
  CurrentBackends = apps:lookup(backends, Hostname),
  OtherBackends = delete_backend_from_list(Backend, CurrentBackends),
  apps:store(backend, Hostname, OtherBackends).

% Delete a backend from the a list
delete_backend_from_list(Backend, CurrentBackends) ->
  lists:filter(fun(B) -> backend_is_same_as(B, Backend) == false end, CurrentBackends).
    
% There must be a bette way to do this, but... this checks to see if the name, host and port
% of the two backends are equal
backend_is_same_as(#backend{name = Name, port = Port, host = Host} = _Backend, 
                    #backend{name = OtherName, port = OtherPort, host = OtherHost} = _OtherBackend) ->
  case Name == OtherName of
    false -> false;
    true ->
      case Port == OtherPort of
        false -> false;
        true ->
          Host == OtherHost
      end
  end.

% Create a new backend from proplists
create_backend_from_proplist(Backend, NewProps) ->
  PropList = ?rec_info(backend, Backend),
  FilteredProplist1 = filter_backend_proplist(PropList, NewProps, []),
  FilteredProplist = new_or_previous_value(FilteredProplist1, PropList, []),
  list_to_tuple([backend|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, backend)]]).

% Choose the new value if the value doesn't exist in a proplist given already,
% otherwise, choose the old value (default)
new_or_previous_value(_NewProplist, [], Acc) -> Acc;
new_or_previous_value(NewProplist, [{K,V}|Rest], Acc) ->
  case proplists:is_defined(K,NewProplist) of
    true -> 
      NewV = proplists:get_value(K, NewProplist),
      new_or_previous_value(NewProplist, Rest, [{K, NewV}|Acc]);
    false ->
      new_or_previous_value(NewProplist, Rest, [{K, V}|Acc])
  end.

% Only choose values that are actually in the proplist
filter_backend_proplist(_BackendProplist, [], Acc) -> Acc;
filter_backend_proplist(BackendProplist, [{K,V}|Rest], Acc) ->
  case proplists:is_defined(K, BackendProplist) of
    false -> filter_backend_proplist(BackendProplist, Rest, Acc);
    true -> filter_backend_proplist(BackendProplist, Rest, [{K,V}|Acc])
  end.