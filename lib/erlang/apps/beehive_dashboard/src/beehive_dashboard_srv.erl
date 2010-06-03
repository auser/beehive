%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jun 26 17:14:22 PDT 2009
%%%-------------------------------------------------------------------

-module (beehive_dashboard_srv).
-author ("David Guttman <dguttman@gmail.com>").
-include ("beehive.hrl").
-include ("common.hrl").
-include ("http.hrl").
%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chat.
-export ([start_link/0]).
-export([start/1, stop/0, loop/2, ws_loop/1]).

-define(TIMEOUT, 20000).

%% External API

start_link() ->
	Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  WwwDir = filename:join([Dir, "priv", "www"]),
	start([{docroot, WwwDir}]).
	
start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
  Port = config:search_for_application_value(dashboard_port, 4998, beehive),
  WebConfig = [{port, Port}, {name, get_name("web")}, {loop, Loop} | Options1],
  erlang:display(WebConfig),
  mochiweb_http:start(WebConfig),
  
  WebSocketLoop = fun (WebSocket) -> ?MODULE:ws_loop(WebSocket) end,
  WsPort = config:search_for_application_value(websocket_dashboard_port, 4997, beehive),
  WebSocketConfig = [{port,WsPort}, {name, get_name("websocket")}, {loop, WebSocketLoop}],
  erlang:display(WebSocketConfig),
  mochiweb_websocket:start(WebSocketConfig).

stop() ->
  mochiweb_http:stop(get_name("web")),
  mochiweb_websocket:stop(get_name("websocket")).

loop(Req, DocRoot) ->
  "/" ++ Path = Req:get(path),
  case Req:get(method) of
    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
      case Path of
        _ ->
          Req:serve_file(Path, DocRoot)
        end;
    'POST' ->
      case Path of
        _ ->
          Req:not_found()
      end;
    _ ->
      Req:respond({501, [], []})
  end.

ws_loop(WebSocket) ->
    %% Get the data sent from the client
    Data = WebSocket:get_data(),
    
    %% Our example...
    case Data of
	%% On initial connect we get this message
	"client-connected" ->
	    WebSocket:send("You are connected!");
	%% Other messages go here
	Other ->
	    Msg = "You Said: " ++ Other,
	    WebSocket:send(Msg)
    end.
%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

get_name(Name) -> erlang:list_to_atom(lists:flatten([erlang:atom_to_list(?MODULE), "_", Name])).