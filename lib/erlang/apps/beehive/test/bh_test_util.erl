-module (bh_test_util).
-author("Ari Lerner <arilerner@mac.com>").
-compile(export_all).
-include ("beehive.hrl").
-include ("common.hrl").

setup() ->
  setup([]).

setup(Proplist) when is_list(Proplist) ->
  Dir = filename:dirname(filename:dirname(code:which(?MODULE))),
  ConfigFile = filename:join([Dir, "test", "fixtures", "beehive.cfg"]),

  application:set_env(beehive, node_type,     proplists:get_value(node_type, Proplist, test_type)),
  application:set_env(beehive, config_file,   proplists:get_value(config_file, Proplist, ConfigFile)),
  application:set_env(beehive, beehive_home,  proplists:get_value(beehive_home, Proplist, "/tmp/beehive/test")),
  application:set_env(beehive, database_dir,  proplists:get_value(database_dir, Proplist, "/tmp/beehive/test/test_db")),
  application:start(sasl),
  beehive:start([{beehive_db_srv, testing}]),

  % erlang:display({beehive_db_srv, init_databases, start}),
  % beehive_db_srv:init_databases(),
  % erlang:display({beehive_db_srv, init_databases, done}),
  timer:sleep(200),
  % We don't need any error output here
  inets:start(),
  ok;

setup(Table) ->
  % beehive_db_srv:start_link(),
  % application:start(sasl),
  setup(),
  clear_table(Table),
  ok.

try_to_fetch_url_or_retry(_Method, _Args, 0) -> failed;
try_to_fetch_url_or_retry(Method, Args, Times) ->
  case bh_test_util:fetch_url(Method, Args) of
    {ok, _Headers, _Body} = T -> T;
    _E -> try_to_fetch_url_or_retry(Method, Args, Times - 1)
  end.

fetch_url(Method, Props) ->
  Host    = proplists:get_value(host, Props, "localhost"),
  Port    = proplists:get_value(port, Props, 4999),
  Path    = proplists:get_value(path, Props, "/"),

  Headers = proplists:get_value(headers, Props, []),

  Params  = lists:flatten(lists:map(fun({Key, Value}) ->
                                        lists:flatten([atom_to_list(Key),
                                                       "=", Value, "&"])
                                    end, proplists:get_value(params, Props, []))),
  case gen_tcp:connect(Host, Port, [binary]) of
    {ok, Sock} ->

      RequestLine =
        lists:flatten(
          [string:to_upper(atom_to_list(Method)),
           " ",
           Path,
           " HTTP/1.0\r\n",
           lists:map(fun({Key, Value}) ->
                         lists:flatten([Key, ": ", Value, "\n"])
                     end, Headers),
           lists:flatten(["Content-Length: ", integer_to_list(erlang:length(Params))]),
           "\r\n\r\n",
           Params,
           "\r\n"]),
      file:write_file(lists:flatten(["tmp",atom_to_list(Method)]), RequestLine),
      gen_tcp:send(Sock, RequestLine),
      request(Sock, []);
    Else ->
      {error, Else}
  end.

request(Sock, Acc) ->
  receive
	  {tcp, Sock, Data} ->
      % Received data
      request(Sock, [binary_to_list(Data)|Acc]);
    {tcp_closed, Sock} ->
      parse_http_request(lists:flatten(lists:reverse(Acc)));
  	{tcp_error, Sock} ->
      {error, Sock};
  	Else ->
  	  erlang:display({got, Else}),
  	  request(Sock, Acc)
  % If there is no activity for a while and the socket has not already closed,
  % we'll assume that the connection is tired and should close, so we'll close it
  after 1000 ->
    {error, timeout}
  end.

parse_http_request(Acc) ->
  [Headers|Body] = string:tokens(Acc, "\r\n"),
  {ok, Headers, Body}.

teardown() ->
  application:set_env(beehive, beehive_home, "/tmp/beehive/test"),
  beehive:stop(),
  ok.

clear_table(Table) ->
  beehive_db_srv:delete_all(Table),
  ok.

start(Count)      -> start(Count, example_cluster_srv, 0, []).
start(Count, Mod) -> start(Count, Mod, 0, []).
start(Count, _Mod, Count, Acc) -> {ok, Acc};
start(Count, Mod, CurrentCount, Acc) ->
  Name = erlang:list_to_atom(lists:flatten(["node", erlang:integer_to_list(CurrentCount)])),
  Seed = case erlang:length(Acc) of
    0 -> undefined;
    _ -> whereis(erlang:hd(Acc))
  end,
  {ok, _NodePid} = Mod:start_named(Name, [{seed, Seed}]),
  start(Count, Mod, CurrentCount + 1, [Name|Acc]).

shutdown([]) -> ok;
shutdown([Pname|Rest]) ->
  Pid = whereis(Pname),
  gen_cluster:cast(Pid, stop),
  try unregister(Pname)
  catch _:_ -> ok
  end,
  shutdown(Rest).

context_run(Count, Fun) ->
  {ok, Nodes} = start(Count),
  Fun(),
  shutdown(Nodes).

% FIXTURE
dummy_app() ->
  Dir = ?BH_ROOT,
  ReposDir = filename:join([Dir, "test", "fixtures", "incredibly_simple_rack_app"]),
  ReposUrl = lists:concat(["file://", ReposDir]),

  % {ok, App} = case apps:find_by_name("test_app") of
  %   not_found ->
  %
  %   App1 ->
  %     {ok, App1}
  % end,
  apps:new(#app{name = "test_app", url = ReposUrl}).

dummy_user() ->
  {ok, User} = case users:find_by_email("test@getbeehive.com") of
    not_found ->
      UserC = #user{email = "test@getbeehive.com", password="test"},
      users:create(UserC);
    U1 -> {ok, U1}
  end,
  User.

% Utils
delete_all(Table) ->
  Pluralized = erlang:list_to_atom(lists:append([erlang:atom_to_list(Table), "s"])),
  lists:map(fun(O) ->
    Pluralized:delete(O)
  end, Pluralized:all()).
  % beehive_db_srv:delete_all(Table).

response_json(Response) ->
  Json = lists:last(Response),
  BodyStruct = mochijson2:decode(Json),
  parse_json_struct(BodyStruct).

parse_json_struct({struct, List}) ->  parse_json_struct(List);
parse_json_struct({Key, Value}) ->
  {binary_to_list(Key), parse_json_struct(Value) };
parse_json_struct(List) when is_list(List) ->
  lists:map( fun(E) ->
                 parse_json_struct(E)
             end, List);
parse_json_struct(Binary) when is_binary(Binary) ->  binary_to_list(Binary);
parse_json_struct(Int) when is_integer(Int) -> Int.

