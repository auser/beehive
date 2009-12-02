%%%-------------------------------------------------------------------
%%% File    : schema.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:32:58 PST 2009
%%%-------------------------------------------------------------------

-module (schema).

-include ("beehive.hrl").
-include ("common.hrl").

-export ([
  install/0, install/1,
  remove/1, initialized/0
]).

initialized() ->
  try
    (catch db:start()),
    lists:map(fun(TableAtom) -> mnesia:table_info(TableAtom, type) end, [app, bee, user, user_app]),
    true
  catch _:_Why -> 
    ?LOG(info, "Creating tables", []),
    false
  end.

install() -> install([node()]).
remove(Node) ->
  (catch mnesia:stop()),
  mnesia:del_table_copy(app, Node).

install(Nodes) when is_list(Nodes) ->
  (catch mnesia:stop()),
  mnesia:delete_schema(Nodes),
  (catch mnesia:create_schema(Nodes)),
  db:start(),
  install_app(Nodes),
  install_bee(Nodes),
  install_user(Nodes),
  % Load the config'd applications
  app_manager:load_static_configs(),
  ok.

install_app(Nodes) ->
  try 
    mnesia:table_info(app, type)
  catch
    exit:_Why ->
      io:format("Creating table app\n"),
      mnesia:create_table(app,[
        {attributes, record_info(fields, app)},
        {type, set},
        {disc_copies, Nodes}
      ]);
    Error ->
      io:format("Error creating mnesia table: ~p\n", [Error]),
      throw(Error)
  end.
  
install_bee(Nodes) ->
  try 
    mnesia:table_info(bee, type)
  catch
    exit:_Why ->
      io:format("Creating table bee\n"),
      mnesia:create_table(bee,[
        {attributes, record_info(fields, bee)},
        {type, set},
        {disc_copies, Nodes}
      ]);
    Error ->
      io:format("Error creating mnesia table: ~p", [Error]),
      throw(Error)
  end.

install_user(Nodes) ->
  try 
    mnesia:table_info(user, type),
    mnesia:table_info(user_app, type)
  catch
    exit:_Why ->
      io:format("Creating table user\n"),
      mnesia:create_table(user,[
        {attributes, record_info(fields, user)},
        {type, set},
        {disc_copies, Nodes}
      ]),
      io:format("Creating table user_app\n"),
      mnesia:create_table(user_app, [
        {attributes, record_info(fields, user_app)},
        {type, set},
        {disc_copies, Nodes}
      ]),
      users:add_root_user();
    Error ->
      io:format("Error creating mnesia table: ~p", [Error]),
      throw(Error)
  end.
