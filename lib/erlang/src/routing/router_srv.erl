%%%-------------------------------------------------------------------
%%% File    : router_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Tue Sep  1 12:12:32 PDT 2009
%%%-------------------------------------------------------------------

-module (router_srv).
-include ("beehive.hrl").
-behaviour(gen_cluster).

%% API
-export([start_link/0, start_link/1]).
-export ([find_application/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_node_joined/3, handle_leave/4]).


-record (app, {
					name,				% Name of the application
					endpoints		% Servers that the app lives on
				}).	
				
-record(state, {
          applications = []
        }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
find_application(AppName) ->
	gen_server:call(?SERVER, {find_application, AppName}).
	
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> start_link([]).

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
init(Args) ->
  RunningApplications = find_local_applications(),
  process_flag(trap_exit, true),
	
	StartArgs = get_start_args(Args),
	
  start_mochiweb(StartArgs),
	
  {ok, #state{
    applications = RunningApplications
  }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({find_application, AppName}, _From, State) ->
	Reply = handle_find_application(AppName, State),
	{reply, Reply, State};
handle_call({list_local_applications}, _From, State) ->
  Reply = handle_list_local_applications(State),
  {reply, Reply, State};
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
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(JoiningPid, Pidlist, State) ->
	io:format(user, "~p:~p handle join called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_node_joined(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------

handle_node_joined(JoiningPid, Pidlist, State) ->
	io:format(user, "~p:~p handle node_joined called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
	{ok, State}.

handle_leave(LeavingPid, Pidlist, Info, State) ->
	io:format(user, "~p:~p handle leave called: ~p, Info: ~p Pidlist: ~p~n", [?MODULE, ?LINE, LeavingPid, Info, Pidlist]),
	{ok, State}.



%%====================================================================
%% HANDLERS
%%====================================================================
handle_find_application(AppName, State) ->
	none.
	
handle_list_local_applications(#state{applications = Apps} = _State) ->
  ?INFO("Applications: ~p~n", [Apps]).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------  
find_local_applications() -> app_discovery:discover_local_apps().


dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

% HANDLE
% Handle the requests
handle("/favicon.ico", Req) -> Req:respond({200, [{"Content-Type", "text/html"}], ""});
handle(Path, Req) ->
	?INFO("Got request for: ~p~n", Path),
	Req:ok({"text/html", "<h3>Not found</h3>"}).

start_mochiweb(Args) ->
  [Port] = Args,
  io:format("Starting mochiweb_http with ~p~n", [Port]),
  mochiweb_http:start([ {port, Port},
                        {loop, fun dispatch_requests/1}]).


%%--------------------------------------------------------------------
%% Function: get_start_args (Args) -> [port]
%% Description: Get the start args from the application env
%%--------------------------------------------------------------------
get_start_args(Args) ->
  Module = case proplists:get_value(module, Args) of
    undefined -> hermes;
    Else -> Else
  end,
  lists:map(fun ({Var, Default}) -> 
  	  case application:get_env(Module, Var) of
        undefined -> Default;
  	    {ok, V} -> V
      end
	  end, [
	        {port, get_env_or_default("ROUTER_PORT", 9991)}
	       ]).
	
get_env_or_default(Env, Default) ->
	case os:getenv(Env) of
		false -> Default;
		E -> E
	end.
	
% Get a clean path
% strips off the query string
clean_path(Path) ->
  case string:str(Path, "?") of
    0 -> Path;
    N -> string:substr(Path, 1, string:len(Path) - (N+1))
  end.

top_level_request(Path) ->
  case string:tokens(Path, "/") of
    [CleanPath|_Others] -> CleanPath;
    [] -> "home"
  end.
