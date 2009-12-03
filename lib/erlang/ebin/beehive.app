{application, beehive,
 [
  {description, "Beehive app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {beehive, []}},
  {env, [
    {node_type, router},
    {seed, []},
    {user_defined_event_handler, undefined},
    {log_path, undefined}
  ]}
 ]
}.