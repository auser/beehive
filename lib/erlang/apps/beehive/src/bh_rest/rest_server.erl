%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jun 26 17:14:22 PDT 2009
%%%-------------------------------------------------------------------

-module (rest_server).
-behaviour(gen_server).

-include ("beehive.hrl").
-include ("common.hrl").
-include ("http.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
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
% TODO: Update port args with config variables
init([]) ->
  Port = config:search_for_application_value(default_app_port, 4999),
  WebServerName = config:search_for_application_value(webserver, mochiweb),
  Settings = [
    "Http beehive rest server",
    {"Port", integer_to_list(Port)}
  ],
  
  printer:banner(Settings),
  
  Dir = ?BH_ROOT,
  Docroot = filename:join([Dir, "priv", "www"]),
  
  WebServer = case WebServerName of
    mochiweb -> mochiweb_http;
    _ -> mochiweb_http
  end,
  
  WebServer:start([ {port, Port},
                    {loop, fun(Req) -> dispatch_requests(WebServerName, Docroot, Req) end}]).


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
  mochiweb_http:stop(),
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
dispatch_requests(WebServerName, Docroot, RawRequest) ->
  {Req, Resp} = case WebServerName of
    mochiweb -> 
      Info = {RawRequest, Docroot},
      {simple_bridge:make_request(mochiweb_request_bridge, Info),
        simple_bridge:make_response(mochiweb_response_bridge, Info)};
    _ -> throw({uh_oh})
  end,
  Path = Req:path(),
  handle(Path, Docroot, Req, Resp).

% Handle the requests
handle("/favicon.ico", _Docroot, _Req, Resp) -> 
  Resp1 = Resp:status_code(200),
  Resp2 = Resp1:header("Content-Type", "text/html"),
  Resp3 = Resp2:data(""),
  Resp3:build_response();
  
handle(Path, Docroot, Req, Resp) ->
  BaseController = lists:concat([top_level_request(Path), "_controller"]),
  CAtom = list_to_atom(BaseController),
  Meth = clean_method(Req:request_method()),
  Data = decode_data_from_request(Req, Meth),
  ControllerPath = parse_controller_path(Path),
  run_controller(Req, Resp, Docroot, CAtom, Meth, [ControllerPath, Data]).

% Call the controller action here
run_controller(Req, Resp, Docroot, ControllerAtom, Meth, Args) ->
  case (catch erlang:apply(ControllerAtom, Meth, Args)) of
    {'EXIT', {undef, Reason}} ->
      % ?LOG(error, "(~p:~p) Undefined method rest server: ~p~n", [?MODULE, ?LINE, Reason]),
      respond_to(Req, Resp, Docroot, Reason);
    {'EXIT', E} -> 
      ?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE, E]),
      erlang:display({error, E}),
      Resp1 = Resp:status_code(503),
      Resp2 = Resp1:header("Content-Type", "text/html"),
      Resp3 = Resp2:data("Nothing to see here"),
      Resp3:build_response();
    {error, Status, Message} when is_integer(Status) ->
      Resp1 = Resp:status_code(Status),
      Resp2 = Resp1:header("Content-Type", "application/json"),
      Resp3 = Resp2:data(?JSONIFY(Message)),
      Resp3:build_response();
    {error, _} = Tuple ->
      % Any errors must be thrown to be caught
      Resp1 = Resp:status_code(404),
      Resp2 = Resp1:header("Content-Type", "application/json"),
      Resp3 = Resp2:data(?JSONIFY(Tuple)),
      Resp3:build_response();
    Body -> 
      respond_to(Req, Resp, Docroot, Body)
  end.

respond_to(Req, Resp, Docroot, Body) ->
  ReturnResp = case string:right(Req:path(), 5) of
    ".json" ->
      Resp1 = Resp:status_code(200),
      Resp2 = Resp1:header("Content-Type", "application/json"),
      Resp2:data(?JSONIFY(Body));
    _ ->
      Resp1 = Resp:header("Content-Type", "text/html"),
      Filename = case Req:path() of
        "/" -> "index.html";
        "/" ++ E -> E
      end,
      serve_file(Filename, Resp1, Docroot, true)
  end,
  ReturnResp:build_response().

% Find the method used as a request. 
% This turns 'GET' into get
clean_method(M) ->
  case M of
    % This is a hack... FOR NOW
    'OPTIONS' -> get;
    _ -> erlang:list_to_atom(string:to_lower(erlang:atom_to_list(M)))
  end.

% parse the controller path
parse_controller_path(CleanPath) ->
  case string:tokens(generalize_request_path(CleanPath), "/") of
    [] -> [];
    [_RootPath|Rest] -> Rest
  end.

% Query about the top level request path is
top_level_request("/") -> home;
top_level_request(Path) ->
  case string:tokens(Path, "/") of
    [CleanPath|_Others] -> generalize_request_path(CleanPath);
    [] -> home
  end.

% generalize request path
generalize_request_path(Path) ->
  case string:rstr(Path, ".json") of
    0 -> Path;
    _ -> 
      % EWWWW
      string:substr(Path, 1, erlang:length(Path) - 5)
  end.


atomize_keys(Data) ->
  lists:flatten(lists:map( fun({K,V}) -> atomize_kv(K,V) end, Data)).

atomize_kv(Key, []) -> 
  try
    case mochijson2:decode(Key) of
      {struct, List} when is_list(List) ->
        lists:map(fun({K,V}) -> 
                      atomize_kv(misc_utils:to_atom(K),
                                 misc_utils:to_list(V)) end, List);
      {BinKey, BinVal} ->
        K = misc_utils:to_atom(BinKey),
        Val = misc_utils:to_list(BinVal),
        {K, Val}
    end
  catch
    error:_Why -> {Key, []}
  end;
atomize_kv(Key, Value) when is_list(Key) -> atomize_kv(list_to_atom(Key), Value);
atomize_kv(Key, Value) when is_atom(Key) -> {Key, Value}.

% Get the data off the request
decode_data_from_request(Req, get) -> atomize_keys(Req:query_params());
decode_data_from_request(Req, delete) ->
  case atomize_keys(Req:query_params()) of
    [] -> atomize_keys(Req:post_params());
    Params -> Params
  end;
decode_data_from_request(Req, _Meth) -> atomize_keys(Req:post_params()).

not_found_web(Resp, Docroot, _Redirect) -> serve_file("not_found.html", Resp:status_code(404), Docroot, false).
serve_file(Path, Resp, Docroot, Redirect) -> 
	RealPath = filename:join([Path]),
  FullPath = filename:join([Docroot, RealPath]),
  case filelib:is_file(FullPath) of
    true -> 
      Resp1 = Resp:status_code(200),
      Resp1:file([[], Path]); % this is dumb... thanks simple_bridge
    _ -> 
      case Redirect of
        true -> not_found_web(Resp, Docroot, false);
        _ -> ?ERROR_HTML(io_lib:format("Error with serving the page: ~p", [FullPath]))
      end
  end.
