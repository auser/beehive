%%%-------------------------------------------------------------------
%%% File    : events_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:49 PST 2009
%%%-------------------------------------------------------------------

-module (events_controller).
-include ("common.hrl").
-include ("beehive.hrl").
-include ("http.hrl").
-export ([get/2, post/2, put/2, delete/2]).

get(_, _Data) -> 
  {events, dashboard_event_handler:get_latest_events()}.

post(_Path, _Data) -> app_error("unhandled").
put(_Path, _Data) -> app_error("unhandled").
delete(_Path, _Data) -> app_error("unhandled").

app_error(Msg) ->
  {error, misc_utils:to_bin(Msg)}.
