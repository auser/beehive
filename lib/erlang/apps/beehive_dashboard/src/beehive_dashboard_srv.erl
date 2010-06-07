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
  send_message_to_all_websockets/1
]).

-export ([
  handle_web_req/2,
  handle_web/4,
  handle_websocket/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% {state, undefined, 4998, 4997, []}.
-record(state, {
  docroot,
  web_port        = 4998,
  live_websockets = []
}).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 20000).

%%====================================================================
%% API
%%====================================================================
new_client_connected(WebSocket) -> 
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
	
	State = #state{
    docroot = Docroot,
    web_port = Port,
    live_websockets = []
  },
  
	start_web_server(State),
	
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
    Socket = WebSocket:get(socket),
    case erlang:port_info(Socket) of
      undefined ->
        % Port not connected anymore 
        client_disconnected(WebSocket);
      _ ->
        JsonMsg = ?JSONIFY(Msg),
        WebSocket:send(JsonMsg)
    end
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
  misultin:stop(),
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
  Loop = fun (Req) -> ?MODULE:handle_web_req(Req, DocRoot) end,
  WebSocketLoop = fun (WebSocket) -> ?MODULE:handle_websocket(WebSocket) end,
  WebConfig = [{port, Port}, {name, ?MODULE}, {loop, Loop}, {ws_loop, WebSocketLoop}],
  misultin:start_link(WebConfig).
  
%% Loops
% Web requests
handle_web_req(Req, Docroot) -> handle_web(Req:get(method), Req:resource([lowercase, urldecode]), Req, Docroot).

handle_web('GET', [], Req, Docroot)       -> serve_file(["index.html"], Req, Docroot);
handle_web('GET', Path, Req, Docroot)   -> serve_file(Path, Req, Docroot);
handle_web('HEAD', Path, Req, Docroot)  -> serve_file(Path, Req, Docroot);
handle_web('POST', _, Req, _Docroot)  -> Req:respond({404, [], []});
handle_web(_, _, Req, Docroot) ->
  not_found_web(Req, Docroot).

not_found_web(Req, Docroot) -> serve_file(["not_found.html"], Req, Docroot).
serve_file(Path, Req, Docroot) -> 
	RealPath = filename:join(Path),
  FullPath = filename:join([Docroot, RealPath]),
  case filelib:is_file(FullPath) of
    true -> Req:file(FullPath);
    _ -> not_found_web(Req, Docroot)
  end.

handle_websocket(Ws) ->
  receive
    {browser, "client-connected"} ->
      ?MODULE:new_client_connected(Ws),
      Ws:send("You are connected!"),
      handle_websocket(Ws);
    {browser, Data} ->
      Ws:send(["received '", Data, "'"]),
      handle_websocket(Ws);
    Msg ->
      erlang:display({got_web_socket, Msg}),
      handle_websocket(Ws)
  after 5000 ->
    handle_websocket(Ws)
  end.
