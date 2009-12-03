%%%-------------------------------------------------------------------
%%% File    : bh_git_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Based on mojombo's egitd
%%% Created :  Wed Dec  2 20:02:41 PST 2009
%%%-------------------------------------------------------------------

-module (bh_git_srv).
-include ("beehive.hrl").
-include ("common.hrl").

-export([start_link/0, init/1]).

start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
  Port = config:search_for_application_value(git_port, 9148, storage),
  LSock = try_listen(Port, 10),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(LSock).
    
try_listen(Port, 0) ->
  ?LOG(error, "Could not listen on the git port: ~p", [Port]);
try_listen(Port, Times) ->
  Res = gen_tcp:listen(Port, [list, {packet, 0}, {active, false}]),
  case Res of
    {ok, LSock} ->
      ?LOG(info, "[git] Listening on port: ~p", [Port]),
      LSock;
    {error, _Reason} ->
      timer:sleep(5000),
      try_listen(Port, Times - 1)
  end.
    
loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> handle_method(Sock) end),
  loop(LSock).
  
handle_method(Sock) ->
  % get the requested host and method
  case gen_tcp:recv(Sock, 0) of
    {ok, Header} ->
      {ok, Host} = extract_host(Header),
      Method = extract_method_name(Header),
  
      % dispatch
      handle_method_dispatch(Method, Sock, Host, Header);
    {error, closed} ->
      ok = gen_tcp:close(Sock)
  end.
  
handle_method_dispatch({ok, "upload-pack"}, Sock, Host, Header) ->
  bh_upload_pack:handle(Sock, Host, Header);
handle_method_dispatch({ok, "receive-pack"}, Sock, Host, Header) ->
  bh_receive_pack:handle(Sock, Host, Header);
handle_method_dispatch(invalid, Sock, _Host, _Header) ->
  gen_tcp:send(Sock, "Invalid method declaration. Upgrade to the latest git.\n"),
  ok = gen_tcp:close(Sock).
  
extract_method_name(Header) ->
  case re:run(Header, "....git[ -]([a-z\-]+) ") of
    {match, Matches} ->
      {Start, Length} = hd(lists:reverse(Matches)),
      {ok, string:substr(Header, Start+1, Length)};
    _Else ->
      invalid
  end.
  
extract_host(Header) ->
  case re:run(string:to_lower(Header), "host=(.*)\^@") of % \000host=[^\000]+\000
    {match, Matches} ->
      {Start, Length} = hd(lists:reverse(Matches)),
      {ok, string:substr(Header, Start+1, Length-1)};
    _Else ->
      {ok, "invalid"}
  end.