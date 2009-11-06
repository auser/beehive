{application, router,
 [
  {description, "Router app"},
  {vsn, "0.1"},
  {id, "router_srv"},
  {modules,      [tcp_socket_server, app_srv]},
  {registered,   [tcp_socket_server, app_srv]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {router, []}},
  {env, [
    {client_port, 8080},
    {routing_parameter, 'Host'},
    {beehive_app_port, 4999},
    {backends, "config/sample.cfg"},
    {kvstore, dict_store},
    {log_path, "/tmp/logs/router.log"}
  ]}
 ]
}.