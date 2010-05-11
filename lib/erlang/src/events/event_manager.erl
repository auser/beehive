%%%-------------------------------------------------------------------
%%% File    : event_manager.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Nov  2 12:15:49 PST 2009
%%%-------------------------------------------------------------------

-module (event_manager).

-include ("beehive.hrl").
-include ("common.hrl").

%% API
-export([start_link/0, stop/0, add_handler/1, notify/1]).

-define(SERVER, ?MODULE).
-define (EVENT_HANDLERS, [
  log_event_handler, 
  node_event_handler,
  app_event_handler, 
  proxy_event_handler, 
  bee_event_handler, 
  db_event_handler,
  user_defined_event_handler]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
  O = gen_event:start_link({local, ?SERVER}),
  % Add the defined event handlers
  lists:map(fun(Handler) -> add_handler(Handler) end, ?EVENT_HANDLERS),
  O.

%%-------------------------------------------------------------------
%% @spec () ->    {ok, Value}
%% @doc Stop
%%      
%% @end
%%-------------------------------------------------------------------
stop() ->
  gen_event:stop(?SERVER).

%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Module) -> 
  case (catch gen_event:add_handler(?SERVER, Module, [])) of
    ok -> ok;
    {'EXIT', E} ->
      io:format("Couldn't add handler: ~p~n", [E]),
      exit(E)
  end.

%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
notify(Event) -> gen_event:notify(?SERVER, Event).