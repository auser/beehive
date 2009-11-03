{application, backend,
 [
  {description, "Backend application"},
  {vsn, "0.1"},
  {id, "backend_srv"},
  {modules,      [backend_srv]},
  {registered,   [backend_srv, backend_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {backend_app, []}},
  {env, [
    {port, 8081},
    {app_dir, "./apps"},
    {hostname, undefined},
    {script_dir, "/opt/beehive/bin"}
  ]}
 ]
}.