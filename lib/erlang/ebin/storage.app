{application, storage,
 [
  {description, "Beehive storage app"},
  {vsn, "0.1"},
  {id, "beehive_storage_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {beehive, []}},
  {env, [
    {git_port, undefined},
    {git_repos_path, undefined}
  ]}
 ]
}.