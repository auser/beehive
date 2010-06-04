%%% beehive_dashboard_srv.erl
%% @author David M. Guttman
%% @copyright 06/03/10 David M. Guttman
%% @doc Dashboard server
-module (beehive_dashboard_srv).
-author("David M. Guttman <davidmguttman@gmail.com>").

-behaviour(gen_server).
-include ("http.hrl").

%% API
-export([
  start_link/0,
  new_client_connected/1,
  client_disconnected/1,
  send_message_to_all_websockets/1,
  loop/2, ws_loop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% {state, undefined, 4998, 4997, []}.
-record(state, {
  docroot,
  web_port        = 4998,
  websocket_port  = 4997,
  live_websockets = []
}).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 20000).

%%====================================================================
%% API
%%====================================================================
new_client_connected(WebSocket) -> 
  erlang:display({called, new_client_connected, WebSocket}),
  gen_server:cast(?SERVER, {new_client_connected, WebSocket}).
client_disconnected(WebSocket) -> gen_server:cast(?SERVER, {client_disconnected, WebSocket}).
send_message_to_all_websockets(Msg) -> gen_server:cast(?SERVER, {send_message_to_all_websockets, Msg}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  Docroot = filename:join([Dir, "priv", "www"]),
  Port = config:search_for_application_value(dashboard_port, 4998, beehive),
  WsPort = config:search_for_application_value(websocket_dashboard_port, 4997, beehive),
	
	State = #state{
    docroot = Docroot,
    web_port = Port,
    websocket_port = WsPort,
    live_websockets = []
  },
    
	start_web_server(State),
	start_websocket_server(State),
	
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send_message_to_all_websockets, Msg}, #state{live_websockets = WebSocketArray} = State) ->
  lists:map(fun(WebSocket) -> 
    JsonMsg = ?JSONIFY(Msg),
    WebSocket:send(JsonMsg)
  end, WebSocketArray),
  {noreply, State};
handle_cast({new_client_connected, WebSocket}, #state{live_websockets = WebSocketArray} = State) ->
  NewWebSocketArray = [WebSocket|WebSocketArray],
  {noreply, State#state{live_websockets = NewWebSocketArray}};
handle_cast({client_disconnected, WebSocket}, #state{live_websockets = WebSocketArray} = State) ->
  NewWebSocketArray = lists:delete(WebSocket, WebSocketArray),
  {noreply, State#state{live_websockets = NewWebSocketArray}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  mochiweb_http:stop(get_name("web")),
  mochiweb_websocket:stop(get_name("websocket")),
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_web_server(#state{docroot = DocRoot, web_port = Port} = _State) ->
  Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
  WebConfig = [{port, Port}, {name, get_name("web")}, {loop, Loop}],
  mochiweb_http:start(WebConfig).
  
start_websocket_server(#state{websocket_port = WsPort} = _State) ->
  WebSocketLoop = fun (WebSocket) -> ?MODULE:ws_loop(WebSocket) end,
  WebSocketConfig = [{port,WsPort}, {name, get_name("websocket")}, {loop, WebSocketLoop}],
  mochiweb_websocket:start(WebSocketConfig).
  
get_name(Name) -> erlang:list_to_atom(lists:flatten([erlang:atom_to_list(?MODULE), "_", Name])).

%% Loops
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
  Data = WebSocket:get_data(),
  
  case Data of
	  %% On initial connect we get this message
	  "new_websocket" ->
	    ?MODULE:new_client_connected(WebSocket),
      WebSocket:send("You are connected!");
	  %% Other messages go here
	  Other ->
	    Msg = "You Said: " ++ Other,
	    WebSocket:send(Msg)
  end.