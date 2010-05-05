{application, router,
 [
  {description, "Beehive router app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {bh_router, []}},
  {env, [
    {client_port, undefined},
    {routing_parameter, undefined},
    {run_rest, undefined},
    {beehive_app_port, undefined},
    {bees, undefined},
    {bee_picker, undefined},
    {bee_strategy, undefined}
  ]}
 ]
}.