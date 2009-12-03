%%%-------------------------------------------------------------------
%%% File    : bh_git_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Based on Tom Preston-Werner's egitd (http://github.com/mojombo/egitd)
%%% Created :  Wed Dec  2 20:02:41 PST 2009
%%%-------------------------------------------------------------------

-module (bh_git_srv).
-include ("beehive.hrl").
-include ("common.hrl").

-export([
  start_link/0, 
  init/0, 
  init/1,
  init_accept/2
]).

start_link()          -> init().
% Start listening on the application port
init()                -> init(config:search_for_application_value(git_port, 9148, storage)).
init(LocalPort) -> 
  Pid = spawn_link(?MODULE, init_accept, [LocalPort, 10]),
  {ok, Pid}.

% accept responses on the port given by the application configuration
init_accept(LPort, 0) -> ?LOG(error, "Could not listen on the git port: ~p", [LPort]);
init_accept(LPort, Count) ->
  SockOpts = [list, {packet, 0}, {active, false}],
	case gen_tcp:listen(LPort, SockOpts) of
	  {ok, ListenSocket} -> 
	    accept(ListenSocket);
	  Error ->
	    ?LOG(error, "There was an error listening to the socket for port ~p: ~p", [LPort, Error]),
	    init_accept(LPort, Count - 1)
	end.

accept(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, ClientSock} ->
      spawn(fun() -> handle_method(ClientSock) end),
	    accept(LSock);
    Error ->
      ?LOG(error, "There was an error accepting the socket ~p: ~p", [LSock, Error]),
      exit(error)
  end.

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