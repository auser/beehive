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
  ?CONTENT_HTML(format_proxy_state());

% get("/status", _Data) ->
%   StatusProplist = app_srv:status(),
%   Apps = proplists:get_value(apps, StatusProplist),
%   _Hostnames = proplists:get_value(hostnames, StatusProplist),
%   
%   Content = lists:map(fun({Name, _App}) -> 
%       {struct, 
%         [{Name, misc_utils:to_bin(length([]))}]
%       }
%     end, Apps),
%   
%   ?LOG(info, "Apps: ~p", [Content]),
%   {json, 200, [], {?MODULE, Content}};
  
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
  
  app_srv:add_application(ConfigProplist),
  
  Out = {added, misc_utils:to_bin(Name)},
  {json, 200, [], Out};
  
post(_UnsupportedPath, _Data) ->
  "POST!!!".
  
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".

% Private
convert_to_struct(RawData) ->
  lists:map(fun({BinKey, BinVal}) ->
      Key = misc_utils:to_atom(BinKey),
      Val = misc_utils:to_list(BinVal),
      {Key, Val}
    end, RawData).

jsonify(JsonifiableBody) ->
  [ ?JSON_ENCODE({
        struct, [
          JsonifiableBody
        ]
    })
  ].

%%%
%%% HTTP server stuff
%%%

format_proxy_state() ->
  Backends = backend:all(),
  State = app_srv:get_proxy_state(),
  [
   "<pre>\n",
   %% From README: insert line here!
   io_lib:format("Proxy start time: ~s\n", [date_util:fmt_date(State#proxy_state.start_time)]),
   io_lib:format("Current time:     ~s\n", [date_util:fmt_date(date_util:now_to_seconds())]),
   io_lib:format("Local TCP port number: ~w\n", [State#proxy_state.local_port]),
   io_lib:format("Connection timeout (seconds): ~w\n", [State#proxy_state.conn_timeout / 1000]),
   io_lib:format("Activity timeout (seconds): ~w\n", [State#proxy_state.act_timeout / 1000]),
   
   "</pre>\n",
   "<table>\n",
   "<tr> ",
   [["<td><b>", X, "</b></td>"] || X <- ["Name", "Host", "Port", "Status",
      "TotalReq", "CurrentReq", "LastErr", "LastErrTime", "TotalTime", "AvgRespTime", 
      "PendingCount", "PacketCount"
    ]],
   "\n",
   format_backend_list(Backends),
   "</table>\n"
  ].

format_backend_list(List) -> format_backend_list(List, []).
format_backend_list([], Acc) -> lists:reverse(Acc);
format_backend_list([B|Bs], Acc) ->
    LastErrTime = if
    B#backend.lasterr_time -> B#backend.lasterr_time;
    true -> 62167219200
  end,
  {L1, L2} = case ?QSTORE:get_queue(?WAIT_DB, B#backend.app_name) of
    empty -> {[], []};
    E -> E
  end,

  #backend_stat{
    total_requests = TotalReq,
    current = CurrentReq,
    total_time = TotalTime,
    average_req_time = AvgTime,
    packet_count = PacketCount
  } = 
    _BackendStat = case stats_srv:backend_dump(B#backend.id) of
    [{_Name, Q}|_] -> Q;
    _ -> stats_srv:new_backend_stat()
  end,

  % PidList = backend_pids:lookup(B#backend.app_name),
  % {Active, Pending} = count_reqs(PidList),
  format_backend_list(Bs, [[
     "<tr> ",
     io_lib:format("<td> ~s </td>", [B#backend.app_name]),
     io_lib:format("<td> ~p </td>", [B#backend.host]),
     io_lib:format("<td> ~w </td>", [B#backend.port]),
     io_lib:format("<td> ~w </td>", [B#backend.status]),
     io_lib:format("<td> ~w </td>", [TotalReq]),
     io_lib:format("<td> ~w </td>", [CurrentReq]),
     io_lib:format("<td> ~w </td>", [B#backend.lasterr]),
     io_lib:format("<td> ~s </td>", [date_util:fmt_date(LastErrTime)]),
     io_lib:format("<td> ~w </td>", [TotalTime]),
     io_lib:format("<td> ~w </td>", [AvgTime]),
     io_lib:format("<td> ~w </td>", [length(L1) + length(L2)]),
     io_lib:format("<td> ~w </td>", [PacketCount]),
     "</tr>\n"
    ]|Acc]).
      
count_reqs(Pidlist) ->
  Active = lists:filter(fun({Status, _, _} = _Item) -> Status == active end, Pidlist),
  Pending = lists:filter(fun({Status, _, _} = _Item) -> Status == pending end, Pidlist),
  {Active, Pending}.
