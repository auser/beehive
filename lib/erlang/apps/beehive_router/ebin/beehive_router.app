{application, beehive_router,
 [
  {description, "Beehive router app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      [
      beehive_router,
      beehive_router_sup,
      bh_node_stats_srv,
      http_request_decoder,
      proxy_handler,
      beehive_request,
      bee_store,
      tcp_socket_server, tcp_socket_server_sup
    ]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {beehive_router, []}},
  {env, [
    {client_port, undefined},
    {routing_parameter, undefined},
    {run_rest, undefined},
    {beehive_default_app_port, undefined},
    {bees, undefined},
    {bee_picker, undefined},
    {bee_strategy, undefined}
  ]}
 ]
}.