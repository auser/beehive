{application, beehive,
 [
  {description, "Beehive app"},
  {vsn, "0.1"},
  {id, "router_srv"},
  {modules,      []},
  {registered,   [tcp_socket_server, bee_srv]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {router, []}},
  {env, [
    {client_port, 8080},
    {routing_parameter, 'Host'},
    {run_rest, true},
    {node_type, router},
    {seed, []},
    {beehive_app_port, 4999},
    {bees, undefined},
    {log_path, "/var/logs/router.log"}
  ]}
 ]
}.