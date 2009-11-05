-define(PROXY_HANDLER, proxy_handler).

% Extra time to give an app instance breathing room
-define (TIME_BUFFER, 20).

% Time period to kill an instance after: defaults to an hour
-define (RUN_INSTANCE_TIME_PERIOD, 3600).

% Get the fields of a record into a proplist
-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))). 

% Messages
-define (NEEDS_BACKEND_MSG, needs_backend).
-define (BACKEND_TIMEOUT_MSG, backend_timeout).
-define (MUST_WAIT_MSG, backend_must_wait).

% STORES
-define (APP_DB, apps_db).
-define (BACKEND_DB, backend_db).
-define (WAIT_DB, waiting_db).
-define (PID2BACKEND_DB, pid_to_backend_db).
-define (BACKEND2PID_DB, backend_to_pid_db).

% EVENTS
-define (EVENT_MANAGER, event_manager).
-define (APP_EVENT_HANDLER, app_event_handler).
-define (LOG_EVENT_HANDLER, log_event_handler).

% Port to start on
-define (STARTING_PORT, 5000).

% Expand when
-define (EXPAND_WHEN_PENDING_REQUESTS, 3).

% Time (in seconds) to check the directory for new apps
-define (CHECK_FOR_NEW_APPS_TIME, 5*1000).
-define (IDLE_TIMEOUT, timer:seconds(30)).
-define (MAX_HEADERS, 100).
% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, (1024*1024)).

% Default KV store
-define (KVSTORE, dict_store).
-define (QSTORE, queue_store).

% Application backend
-record (backend, {
  name,
  host,
  port,
  start_time,               % started
  pid,                      % pid of os port process
  lastresp_time = 0,
  lasterr       = 0,
  lasterr_time  = 0,
  act_time      = 0,
  status        = ready,    % pending | ready | broken
  maxconn       = 10,
  act_count     = 0
}).

% Application configuration
-record (app, {
  name,
  backends,
  cmd,
  path,
  url,
  concurrency,
  timeout,
  min_instances,
  max_instances,
  updated_at,
  user,
  group,
  start_command,
  stop_command
}).

-define(BUFSIZ, (128*1024)).

%% Overall proxy_state of the proxy
-record(proxy_state, {
  local_port,				          % Local TCP port number
  local_host,                 % Local host ip tuple or name
  conn_timeout = (1*1000),		% Connection timeout (ms)
  act_timeout = (120*1000),		% Activity timeout (ms)
  acceptor,				            % Pid of listener proc
  start_time,				          % Proxy start timestamp
  to_timer 				            % Timeout timer ref
}).

-record (http_request, {
  client_socket,
  version = {1,1},
  headers = [],
  method,
  path
}).