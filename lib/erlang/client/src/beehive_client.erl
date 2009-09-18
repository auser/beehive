%%%-------------------------------------------------------------------
%%% File    : beehive_client.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Sep 18 00:51:55 PDT 2009
%%%-------------------------------------------------------------------

-module (beehive_client).
-include ("beehive.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export ([register_application/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        port
        }).
        
-define(SERVER, ?MODULE).
-define (MAXLINE, 100).
-define (MAX_ATTEMPTS, 5).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Proggie) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Proggie, []).

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
  process_flag(trap_exit, true),
  
  spawn_link(fun() -> connect_to_router(0) end),
  
  timer:sleep(1000),
  
  ExtProg = start_cmd(),
  ?TRACE("Opening port to: ~p with ~p~n", [ExtProg, ?MODULE]),
  Port = open_port({spawn, ExtProg}, [stream, {line, ?MAXLINE}]),
  {ok, #state{port = Port}}.

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
%%-------------------------------------------------------------------
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
  ?TRACE("Got an exit: ~p~n", [Reason]),
  {stop, {port_terminated, Reason}, State};
handle_info(Info, State) ->
  ?TRACE("Got info: ~p~n", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate({port_terminated, _Reason}, #state{port = Port} = _State) ->
  port_close(Port),
  ok;
terminate(_Reason, #state{port = Port} = _State) ->
  port_close(Port).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_cmd() ->
  case os:getenv("BEEHIVE_CLIENT_CMD") of
    false ->
      case application:get_env(beehive, external_app) of
        {ok, V} -> V;
        _ -> "echo"
      end;
    V -> V
  end.

% Register this application
register_application(Args) ->
  gen_server:call(app_registry_pid(), {register_application, Args}).

app_registry_pid() ->
  global:whereis_name(app_registry_srv).

% Router nodes
router_node() -> get_router_node_from_env().

get_router_node_from_env() ->
  case os:getenv("ROUTER_NODE") of 
      false -> localnode(beehive_router);
      Server -> Server
  end.
    
router_node(Hostname)   ->  case Hostname of
  H when is_atom(H) -> H;
  Else -> list_to_atom(Else)
end.

ping_router() -> 
  net_adm:ping(router_node()).
  
connect_to_router(Attempts) ->  
  case Attempts > ?MAX_ATTEMPTS of
    true -> 
      ?TRACE("Could not connect to the router~n", []);
    false ->
      timer:sleep(2000),
      case ping_router() of
        pong ->
          connect_to_router(0);
        pang -> 
          ?TRACE("Lost connection with router. Trying to regain connection to ~p~n", [router_node()]),
          connect_to_router(Attempts+1)
      end
  end.
  
localnode(Name) ->
    list_to_atom(lists:append(atom_to_list(Name), lists:dropwhile(fun (E) -> E =/= $@ end, atom_to_list(node())))).