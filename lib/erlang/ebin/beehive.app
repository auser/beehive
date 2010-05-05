{application, beehive,
 [
  {description, "Beehive app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {beehive, []}},
  {env, [
    {node_type, router},
    {config_file, undefined},
    {seed, []},
    {user_defined_event_handler, undefined},
    {log_level, 0},
    {log_path, undefined}
  ]}
 ]
}.