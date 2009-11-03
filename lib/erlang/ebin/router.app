{application, router,
 [
  {description, "Router app"},
  {vsn, "0.1"},
  {id, "router_srv"},
  {modules,      [http_client_srv, app_srv]},
  {registered,   [http_client_srv, app_srv]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {router, []}},
  {env, [
    {client_port, 8080},
    {backends, "config/sample.cfg"},
    {kvstore, dict_store},
    {log_path, "/tmp/logs/router.log"}
  ]}
 ]
}.