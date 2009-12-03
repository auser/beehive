{application, node,
 [
  {description, "Beehive node app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {beehive, []}},
  {env, [
  ]}
 ]
}.