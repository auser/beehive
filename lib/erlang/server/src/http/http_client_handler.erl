-module (http_client_handler).

-export ([connect_server_and_client/2]).

connect_server_and_client(Server, Client) ->
	io:format("Server: ~p and Client: ~p~n", [Server, Client]),
	ok.