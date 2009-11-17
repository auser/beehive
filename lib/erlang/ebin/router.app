{application, router,
 [
  {description, "Router app"},
  {vsn, "0.1"},
  {id, "router_srv"},
  {modules,      [tcp_socket_server, backend_srv]},
  {registered,   [tcp_socket_server, backend_srv]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {router, []}},
  {env, [
    {client_port, 8080},
    {routing_parameter, 'Host'},
    {run_beehive, true},
    {seed, []},
    {beehive_app_port, 4999},
    {backends, "config/sample.cfg"},
    {log_path, "/var/logs/router.log"}
  ]}
 ]
}.