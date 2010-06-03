%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jun 26 17:14:22 PDT 2009
%%%-------------------------------------------------------------------

-module (beehive_dashboard_srv).
-author ("David Guttman <dguttman@gmail.com>").
-include ("beehive.hrl").
-include ("common.hrl").
-include ("http.hrl").
%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chat.
-export ([start_link/0]).
-export([start/1, stop/0, loop/2]).

-define(TIMEOUT, 20000).

%% External API

start_link() ->
	Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  WwwDir = filename:join([Dir, "priv", "www"]),
	start([{docroot, WwwDir}]).
	
start(Options) ->
	erlang:display({starting, beehive_dashboard_srv, get_option(docroot, Options)}),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    mochiweb_http:start([{port, 4998}, {name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

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
%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
