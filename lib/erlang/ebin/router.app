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
    {client_port, 8080},
    {routing_parameter, 'Host'},
    {run_rest, true},
    {beehive_app_port, 4999},
    {bees, undefined},
    {bee_picker, undefined},
    {bee_strategy, undefined}
  ]}
 ]
}.