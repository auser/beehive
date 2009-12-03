{application, router,
 [
  {description, "Beehive router app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {beehive, []}},
  {env, [
    {git_port, 9418},
    {git_repos_path, undefined},
  ]}
 ]
}.