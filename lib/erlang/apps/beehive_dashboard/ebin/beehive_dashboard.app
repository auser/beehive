{application, beehive_dashboard,
 [
  {description, "Beehive dashboard app"},
  {vsn, "0.1"},
  {id, "beehive_dashboard_srv"},
  {modules,      [
      beehive_dashboard, beehive_dashboard_srv, beehive_dashboard_sup
    ]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {beehive_dashboard, []}},
  {env, []}
 ]
}.