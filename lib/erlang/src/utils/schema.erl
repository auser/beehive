%%%-------------------------------------------------------------------
%%% File    : schema.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 14:32:58 PST 2009
%%%-------------------------------------------------------------------

-module (schema).

-include ("router.hrl").

-export ([
  install/0,
  install/1
]).

install() -> install([node()]).

install(Nodes) when is_list(Nodes) ->
  (catch mnesia:stop()),
  mnesia:delete_schema(Nodes),
  (catch mnesia:create_schema(Nodes)),
  mnesia:start(),
  install_app(Nodes),
  install_backend(Nodes),
  install_backend_pid(Nodes),
  ok.

install_app(Nodes) ->
  try 
    mnesia:table_info(app, type)
  catch
    exit:_Why ->
      io:format("Creating table app\n"),
      mnesia:create_table(app,[
        {attributes, record_info(fields, app)},{type, set},{disc_copies, Nodes}
      ]);
    Error ->
      io:format("Error creating mnesia table: ~p\n", [Error]),
      throw(Error)
  end.
  
install_backend(Nodes) ->
  try 
    mnesia:table_info(backend, type)
  catch
    exit:_Why ->
      io:format("Creating table backend\n"),
      mnesia:create_table(backend,[
        {attributes, record_info(fields, backend)},{type, bag},{disc_copies, Nodes}
      ]);
    Error ->
      io:format("Error creating mnesia table: ~p", [Error]),
      throw(Error)
  end.

install_backend_pid(Nodes) ->
  try 
    mnesia:table_info(backend_pid, type)
  catch
    exit:_Why ->
      io:format("Creating table backend_pid\n"),
      mnesia:create_table(backend_pid,[
        {attributes, record_info(fields, backend_pid)},{type, set},{disc_copies, Nodes}
      ]);
    Error ->
      io:format("Error creating mnesia table: ~p", [Error]),
      throw(Error)
  end.
  