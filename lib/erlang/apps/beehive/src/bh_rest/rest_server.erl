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
  Port = config:search_for_application_value(app_port, 4999, router),
  WebServerName = config:search_for_application_value(webserver, mochiweb, router),
  Settings = [
    "Http beehive rest server",
    {"Port", integer_to_list(Port)}
  ],
  
  printer:banner(Settings),
  
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
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
  handle(Path, Req, Resp).

% Handle the requests
handle("/favicon.ico", _Req, Resp) -> 
  Resp:status_code(200),
  Resp:header("Content-Type", "text/html"),
  Resp:data(""),
  Resp:build_response();

handle(Path, Req, Resp) ->
  BaseController = lists:concat([top_level_request(Path), "_controller"]),
  CAtom = list_to_atom(BaseController),
  ControllerPath = parse_controller_path(Path),
  
  Data = lists:flatten([Req:query_params(), decode_data_from_request(Req)]),
  
  case CAtom of
    home ->
      Resp1 = Resp:status_code(200),
      Resp2 = Resp1:header("Content-Type", "text/html"),
      Resp3 = Resp2:data(?ERROR_HTML("Uh oh")),
      Resp3:build_response();
    ControllerAtom -> 
      Meth = clean_method(Req:request_method()),
      run_controller(Resp, ControllerAtom, Meth, [ControllerPath, Data])
  end.

% Call the controller action here
run_controller(Resp, ControllerAtom, Meth, Args) ->
  case (catch erlang:apply(ControllerAtom, Meth, Args)) of
    {'EXIT', {undef, _}} = E ->
      ?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE,E]),
      Resp1 = Resp:status_code(200),
      Resp2 = Resp1:header("Content-Type", "text/html"),
      Resp3 = Resp2:data("Unimplemented controller. There is nothing to see here, go back from where you came"),
      Resp3:build_response();
    {'EXIT', E} -> 
      ?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE, E]),
      Resp1 = Resp:status_code(200),
      Resp2 = Resp1:header("Content-Type", "text/html"),
      Resp3 = Resp2:data("Nothing to see here"),
      Resp3:build_response();
    Body -> 
      Resp1 = Resp:status_code(200),
      Resp2 = Resp1:header("Content-Type", "text/json"),
      Resp3 = Resp2:data(?JSONIFY(Body)),
      Resp3:build_response()
  end.

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
  case string:tokens(CleanPath, "/") of
    [] -> [];
    [_RootPath|Rest] -> Rest
  end.

% Get the query params from the path
% query_params(Path) ->
%   case string:str(Path, "?") of
%     0 -> "";
%     N -> string:substr(Path, N+1)
%   end.

% Query about the top level request path is
top_level_request(Path) ->
  case string:tokens(Path, "/") of
    [CleanPath|_Others] -> CleanPath;
    [] -> "home"
  end.

% Convert each of the binary data proplists into a valid proplist
% from {<<name>>, <<value>>} to {name, value}
convert_to_struct(RawData) ->
  lists:map(fun({BinKey, BinVal}) ->
    case BinVal of
      {struct, Arr} -> 
        Key = misc_utils:to_atom(BinKey),
        {Key, convert_to_struct(Arr)};
      _ ->
        Key = misc_utils:to_atom(BinKey),
        Val = misc_utils:to_list(BinVal),
        {Key, Val}
    end
  end, RawData).

% Get the data off the request
decode_data_from_request(Req) ->
  RecvBody = Req:request_body(),
  Data = case RecvBody of
    undefined -> erlang:list_to_binary("{}");
    <<>> -> erlang:list_to_binary("{}");
    Bin -> Bin
  end,
  case mochijson2:decode(Data) of
    {struct, Struct} -> convert_to_struct(Struct);
    B -> convert_to_struct(B)
  end.