{application, bee,
 [
  {description, "Beehive bee app"},
  {vsn, "0.1"},
  {id, "bh_node_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {bh_node, []}},
  {env, [
    {app_dir, undefined}
  ]}
 ]
}.