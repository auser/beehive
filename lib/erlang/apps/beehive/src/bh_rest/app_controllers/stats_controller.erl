%%%-------------------------------------------------------------------
%%% File    : stats_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Nov  5 02:14:23 PST 2009
%%%-------------------------------------------------------------------

-module (stats_controller).

-include ("beehive.hrl").
-include ("common.hrl").
-include ("http.hrl").

-export ([get/2, post/2, put/2, delete/2]).
    
% PATH HANDLING
get([], _Data) ->
  format_proxy_state();

get(_UnsupportedPath, _Data) ->
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
  
  apps:create(ConfigProplist),
  
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
  Backends = bees:all(),
  State = bee_store:get_proxy_state(),
  [
    [
      {"proxy_start_time", State#proxy_state.start_time},
      {"current_time", date_util:now_to_seconds()},
      {"local_port", State#proxy_state.local_port},
      {"connection_timeout", (State#proxy_state.conn_timeout / 1000)},
      {"activity_timeout", (State#proxy_state.act_timeout / 1000)}
    ],
    {"bees", format_bee_list(Backends)}
  ].

format_bee_list(List) -> format_bee_list(List, []).
format_bee_list([], Acc) -> lists:reverse(Acc);
format_bee_list([B|Bs], Acc) ->
  {L1, L2} = case ?QSTORE:get_queue(?WAIT_DB, B#bee.app_name) of
    empty -> {[], []};
    E -> E
  end,

  #bee_stat{
    total_requests = TotalReq,
    current = CurrentReq,
    total_time = TotalTime,
    average_req_time = AvgTime,
    packet_count = PacketCount,
    bytes_received = RecvBytes
  } = 
    _BackendStat = case bh_bee_stats_srv:bee_dump(B#bee.id) of
    [{_Name, Q}|_] -> Q;
    _ -> bh_bee_stats_srv:new_bee_stat()
  end,

  % PidList = bee_pids:lookup(B#bee.app_name),
  % {Active, Pending} = count_reqs(PidList),
  format_bee_list(Bs, [[
    {"app_name", B#bee.app_name},
    {"host", B#bee.host},
    {"port", B#bee.port},
    {"status", B#bee.status},
    {"last_response", B#bee.lastresp_time},
    {"last_err_time", B#bee.lasterr_time},
    {"average_req_time", AvgTime},
    {"pending_requests", (length(L1) + length(L2))},
    {"total_requests", TotalReq},
    {"current_requests", CurrentReq},
    {"total_time", TotalTime},
    {"packet_count", PacketCount},
    {"bytes_received", RecvBytes}
  ]|Acc]).
      
