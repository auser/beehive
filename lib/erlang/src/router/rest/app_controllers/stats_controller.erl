%%%-------------------------------------------------------------------
%%% File    : stats_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Nov  5 02:14:23 PST 2009
%%%-------------------------------------------------------------------

-module (stats_controller).

-include ("router.hrl").
-include ("common.hrl").
-include ("http.hrl").

-export ([get/1, post/2, put/2, delete/2]).
    
% PATH HANDLING
get([]) ->
  format_proxy_state();

get(_UnsupportedPath) ->
  "GET".

post("/new", Data) ->
  
  Name      = proplists:get_value(name, Data),
  Path      = proplists:get_value(path, Data),
  Url       = proplists:get_value(url, Data),
  Hostname  = proplists:get_value(hostname, Data),
  Instances = proplists:get_value(instances, Data),
  Timeout   = proplists:get_value(timeout, Data),
  MinInst   = proplists:get_value(min_instances, Data),
  MaxInst   = proplists:get_value(max_instances, Data),
  MinInst   = proplists:get_value(min_instances, Data),
  StartCmd  = proplists:get_value(start_command, Data),
  StopCmd   = proplists:get_value(stop_command, Data),
  
  ConfigProplist = [
    {name, Name},
    {path, Path},
    {url, Url},
    {hostname, Hostname},
    {instances, Instances},
    {timeout, Timeout},
    {min_instances, MinInst},
    {max_instances, MaxInst},
    {start_command, StartCmd},
    {stop_command, StopCmd}
  ],
  
  backend_srv:add_application(ConfigProplist),
  
  Out = {added, misc_utils:to_bin(Name)},
  {json, 200, [], Out};
  
post(_UnsupportedPath, _Data) ->
  "POST!!!".
  
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".

%%%
%%% HTTP server stuff
%%%

format_proxy_state() ->
  Backends = backend:all(),
  State = backend_srv:get_proxy_state(),
  StateHeaders = ?BINIFY([
    {"proxy_start_time", date_util:fmt_date(State#proxy_state.start_time)},
    {"current_time", date_util:fmt_date(date_util:now_to_seconds())},
    {"local_port", State#proxy_state.local_port},
    {"connection_timeout", (State#proxy_state.conn_timeout / 1000)},
    {"activity_timeout", (State#proxy_state.act_timeout / 1000)}
  ]),
  [
    {struct, StateHeaders},
    {struct, [{"backends", format_backend_list(Backends)}]}
  ].

format_backend_list(List) -> format_backend_list(List, []).
format_backend_list([], Acc) -> lists:reverse(Acc);
format_backend_list([B|Bs], Acc) ->
  {L1, L2} = case ?QSTORE:get_queue(?WAIT_DB, B#backend.app_name) of
    empty -> {[], []};
    E -> E
  end,

  #backend_stat{
    total_requests = TotalReq,
    current = CurrentReq,
    total_time = TotalTime,
    average_req_time = AvgTime,
    packet_count = PacketCount,
    bytes_received = RecvBytes
  } = 
    _BackendStat = case stats_srv:backend_dump(B#backend.id) of
    [{_Name, Q}|_] -> Q;
    _ -> stats_srv:new_backend_stat()
  end,

  % PidList = backend_pids:lookup(B#backend.app_name),
  % {Active, Pending} = count_reqs(PidList),
  format_backend_list(Bs, [{struct, ?BINIFY([
    {"app_name", B#backend.app_name},
    {"host", B#backend.host},
    {"port", B#backend.port},
    {"status", B#backend.status},
    {"last_err_time", B#backend.lasterr_time},
    {"average_req_time", AvgTime},
    {"pending_requests", (length(L1) + length(L2))},
    {"total_requests", TotalReq},
    {"current_requests", CurrentReq},
    {"total_time", TotalTime},
    {"packet_count", PacketCount},
    {"bytes_received", RecvBytes}
  ])}|Acc]).
      
