{application, beehive,
 [
  {description, "Beehive app"},
  {vsn, "0.1"},
  {id, "beehive_srv"},
  {modules,      [
                  beehive, beehive_sup, app_manager,
                  % Events
                  app_event_handler, bee_event_handler, db_event_handler, log_event_handler, node_event_handler,
                  proxy_event_handler, user_defined_event_handler, dashboard_event_handler, user_event_handler,
                  % Models
                  apps, bees, user_apps, users,
                  % Rest interface
                  rest_server, rest_server_sup,
                  apps_controller, auth_controller, bees_controller, git_controller,
                  home_controller, nodes_controller, stats_controller, system_controller,
                  users_controller, events_controller,
                  % UI
                  beehive_control,
                  % Utils
                  bh_file_utils, misc_utils, reloader, sanity_checks, sha1, slugger,
                  bh_host, bh_md5, bin_utils, config,
                  date_util, db, string_utils, yaml,
                  event_manager, printer, port_handler, node_manager, 
                  auth_utils,bee_strategies,bh_bee_stats_srv,
                  web_utils, babysitter_integration, beehive_builder,
                  % Launchers
                  app_launcher_fsm, app_killer_fsm,
                  % App handler
                  app_handler,
                  % Storage
                  beehive_git_srv, beehive_storage_srv,
                  % Stores
                  beehive_db_srv, db_mnesia_adapter,
                  queue_store
                ]},
  {registered,   []},
  {applications, [kernel, stdlib, os_mon]},
  {mod, {beehive, []}},
  {env, [
    {node_type, undefined},
    {config_file, undefined},
    {seed, []},
    {user_defined_event_handler, undefined},
    {log_level, undefined},
    {log_path, undefined}
  ]}
 ]
}.