%%%-------------------------------------------------------------------
%%% File    : router.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Oct  8 18:29:29 PDT 2009
%%%-------------------------------------------------------------------

-module (router).
-include ("router.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  case router_sup:start_link([]) of
    {ok, Pid} -> 
      ?EVENT_MANAGER:add_handler(?APP_EVENT_HANDLER),
      ?EVENT_MANAGER:add_handler(?LOG_EVENT_HANDLER),
      {ok, Pid};
    Error -> Error
  end.

stop(_State) -> ok.
