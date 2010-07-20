-module (rest_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  rest_server:start_link(),
  timer:sleep(100),
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_connectable/0,
        fun test_apps/0
      ]
    }
  }.

test_connectable() ->
  {ok, ReturnedData} = fetch_url(get, [{host, "127.0.0.1"}, {port, 4999}, {path, "/apps.json"}, {headers, [{host, "beehive"}]}]),
  Data = lists:flatten(ReturnedData),
  ?assertEqual("HTTP/1.0 200 OK", string:sub_string(Data, 1, 15)),
  passed.

test_apps() ->
  {ok, ReturnedData} = fetch_url(get, [{host, "127.0.0.1"}, {port, 4999}, {path, "/apps.json"}, {headers, [{host, "beehive"}]}]),
  Data = lists:flatten(ReturnedData),
  ?assertEqual("HTTP/1.0 200 OK", string:sub_string(Data, 1, 15)),
  passed.

fetch_url(Method, Props) ->
  Host    = proplists:get_value(host, Props, "localhost"),
  Port    = proplists:get_value(port, Props, undefined),
  Path    = proplists:get_value(path, Props, "/"),

  Headers = proplists:get_value(headers, Props, []),
  
  {ok, Sock} = gen_tcp:connect(Host, Port, [list]),
  
  RequestLine = lists:flatten([string:to_upper(atom_to_list(Method)), " ", Path, " HTTP/1.0\r\n", 
                lists:map(fun({Key, Value}) ->
                  lists:flatten([string:to_upper(atom_to_list(Key)), ": ", Value, "\n"])
                end, Headers), "\r\n"]),
  gen_tcp:send(Sock, RequestLine),
  proxy_loop(Sock, []).
  
proxy_loop(Sock, Acc) ->
  receive
	  {tcp, Sock, Data} ->
      % Received data
      proxy_loop(Sock, [Data|Acc]);
    {tcp_closed, Sock} ->
      {ok, lists:reverse(Acc)};
  	{tcp_error, Sock} ->
      {error, Sock};
  	_Else -> proxy_loop(Sock, Acc)
  % If there is no activity for a while and the socket has not already closed, 
  % we'll assume that the connection is tired and should close, so we'll close it
  after 1000 ->
    {ok, lists:reverse(Acc)}
  end.
