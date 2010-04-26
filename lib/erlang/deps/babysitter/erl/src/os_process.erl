%%% os_process
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 2010 Ari Lerner <arilerner@mac.com>
%% @doc Every OsPid gets associated with this process, started here
-module (os_process).
-include ("babysitter.hrl").

-export ([
  start/4,
  notify_ospid_owner/2,
  process_owner_died/3
]).

start(Pid, OsPid, Parent, Debug) -> ospid_init(Pid, OsPid, Parent, Debug).

%%----------------------------------------------------------------------
%% @spec (Pid, OsPid::integer(), Parent, Debug::boolean()) -> void()
%% @doc Every OsPid is associated with an Erlang process started with
%%      this function. The `Parent' is the ?MODULE port manager that
%%      spawned this process and linked to it. `Pid' is the process
%%      that ran an OS command associated with OsPid. If that process
%%      requested a link (LinkType = 'link') we'll link to it.
%% @end
%% @private
%%----------------------------------------------------------------------
ospid_init(Pid, OsPid, Parent, Debug) ->
  process_flag(trap_exit, true),
  link(Pid),
  ospid_loop({Pid, OsPid, Parent, Debug}).

ospid_loop({Pid, OsPid, Parent, Debug} = State) ->
  receive
    {{From, Ref}, ospid} ->
      From ! {Ref, {ok, OsPid}},
      ospid_loop(State);
    {stop} ->
      process_owner_died(Pid, normal, State);
    {'DOWN', OsPid, {exit_status, Status}} ->
      ?DBG(Debug, "~w ~w got down message (~w)\n", [self(), OsPid, status(Status)]),
      % OS process died
      exit({exit_status, Status});
    {'EXIT', Pid, Reason} ->
      % Pid died
      ?DBG(Debug, "~w ~w got exit from linked ~w: ~p\n", [self(), OsPid, Pid, Reason]),
      exit({owner_died, Reason});
    {'EXIT', Parent, Reason} ->
      % Port program died
      ?DBG(Debug, "~w ~w got exit from parent ~w: ~p\n", [self(), OsPid, Parent, Reason]),
      exit({port_closed, Reason});
    Other ->
      error_logger:warning_msg("~w - unknown msg: ~p\n", [self(), Other]),
      ospid_loop(State)
  end.

status(Status) when is_integer(Status) ->
  case {Status band 16#FF00 bsr 8, (Status band 128) =:= 128, Status band 127} of
    {Stat, _, 0}      -> {status, Stat};
    {_, Core, Signal} -> {signal, Signal, Core}
  end.


%%-------------------------------------------------------------------
%% @spec (OsPid::int(), Status::integer()) ->    ok
%% @doc Notify the process owner parent that the OsPid died
%% @private
%% @end
%%-------------------------------------------------------------------
notify_ospid_owner(OsPid, Status) ->
  % See if there is a Pid owner of this OsPid. If so, sent the 'DOWN' message.
  case ets:lookup(?PID_MONITOR_TABLE, OsPid) of
    [{_OsPid, Pid}] ->
      unlink(Pid),
      Pid ! {'DOWN', OsPid, {exit_status, Status}},
      ets:delete(?PID_MONITOR_TABLE, {Pid, OsPid}),
      ets:delete(?PID_MONITOR_TABLE, {OsPid, Pid});
    [] ->
      %error_logger:warning_msg("Owner ~w not found\n", [OsPid]),
      ok
  end.

%%-------------------------------------------------------------------
%% @spec (Pid::int(), Reason::string(), State) ->    ok
%% @doc Notify the parent that the parent process died
%% @private
%% @end
%%-------------------------------------------------------------------
process_owner_died(Pid, _Reason, State) ->
  case ets:lookup(?PID_MONITOR_TABLE, Pid) of
    [{_Pid, OsPid}] when is_integer(OsPid) ->
      ?DBG(State#state.debug, "Pid ~p died. Killing linked OsPid ~w\n", [Pid, OsPid]),
      ets:delete(?PID_MONITOR_TABLE, {Pid, OsPid}),
      ets:delete(?PID_MONITOR_TABLE, {OsPid, Pid}),
      erlang:port_command(State#state.port, term_to_binary({0, {kill, OsPid}}));
    _ ->
      ok 
  end.
