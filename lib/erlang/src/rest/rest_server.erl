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
  Port = config:search_for_application_value(beehive_app_port, 4999, router),
  Settings = [
    "Http beehive rest server",
    {"Port", integer_to_list(Port)}
  ],
  printer:banner(Settings),
  
  mochiweb_http:start([ {port, Port},
                        {loop, fun dispatch_requests/1}]).


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
dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

% Handle the requests
handle("/favicon.ico", Req) -> Req:respond({200, [{"Content-Type", "text/html"}], ""});

handle(Path, Req) ->
  BaseController = lists:concat([top_level_request(Path), "_controller"]),
  CAtom = list_to_atom(BaseController),
  ControllerPath = parse_controller_path(Path),
  
  QueryString = lists:map(fun({K,V}) -> {misc_utils:to_atom(K), misc_utils:to_list(V)} end, Req:parse_qs()),
  Data = lists:flatten([QueryString|decode_data_from_request(Req)]),
  
  case CAtom of
    home ->
      IndexContents = ?ERROR_HTML("Uh oh"),
      Req:ok({"text/html", IndexContents});
    ControllerAtom -> 
    Meth = clean_method(Req:get(method)),
    run_controller(Req, ControllerAtom, Meth, [ControllerPath, Data])
  end.

% Call the controller action here
run_controller(Req, ControllerAtom, Meth, Args) ->
  case (catch erlang:apply(ControllerAtom, Meth, Args)) of
    {'EXIT', {undef, _}} = E ->
      ?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE,E]),
      Req:ok({"text/html", "Unimplemented controller. There is nothing to see here, go back from where you came"});
    {'EXIT', E} -> 
      ?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE, E]),
      Req:not_found();
    Body -> 
      Req:ok({"text/json", ?JSONIFY(Body)})
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

% Get a clean path
% strips off the query string
clean_path(Path) ->
  case string:str(Path, "?") of
    0 -> Path;
    N -> string:substr(Path, 1, N - 1)
  end.

% Get the query params from the path
query_params(Path) ->
  case string:str(Path, "?") of
    0 -> "";
    N -> string:substr(Path, N+1)
  end.

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
  RecvBody = Req:recv_body(),
  Data = case RecvBody of
    undefined -> erlang:list_to_binary("{}");
    <<>> -> erlang:list_to_binary("{}");
    Bin -> Bin
  end,
  case mochijson2:decode(Data) of
    {struct, Struct} -> convert_to_struct(Struct);
    B -> convert_to_struct(B)
  end.