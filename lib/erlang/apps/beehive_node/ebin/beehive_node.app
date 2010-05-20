{application, beehive_node,
 [
  {description, "Beehive bee app"},
  {vsn, "0.1"},
  {id, "bh_node_srv"},
  {modules,      [
      app_handler, app_utils,
      beehive_node, beehive_node_sup
    ]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {beehive_node, []}},
  {env, [
    {app_dir, undefined}
  ]}
 ]
}.