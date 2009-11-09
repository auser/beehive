%%%-------------------------------------------------------------------
%%% File    : event_manager.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Nov  2 12:15:49 PST 2009
%%%-------------------------------------------------------------------

-module (event_manager).

-include ("router.hrl").

%% API
-export([start_link/0, add_handler/1, notify/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
  O = gen_event:start_link({local, ?SERVER}),
  % Add the defined event handlers
  lists:map(fun(Handler) -> add_handler(Handler) end, ?EVENT_HANDLERS),
  O.

%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Module) -> gen_event:add_handler(?SERVER, Module, []).

%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
notify(Event) -> gen_event:notify(?SERVER, Event).