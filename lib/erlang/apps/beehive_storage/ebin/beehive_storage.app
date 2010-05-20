{application, beehive_storage,
 [
  {description, "Beehive storage app"},
  {vsn, "0.1"},
  {id, "beehive_storage_srv"},
  {modules,      [
      beehive_git_srv, beehive_storage, beehive_storage_srv, beehive_storage_sup
    ]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {beehive_storage, []}},
  {env, [
    {git_port, undefined},
    {git_repos_path, undefined},
    {git_store, undefined},
    {squashed_storage, undefined},
    {scratch_disk, undefined}
  ]}
 ]
}.