-module (bees).

-include ("beehive.hrl").
-include ("common.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

% DATABASE STUFF
-export ([
  create/1,
  read/1,
  save/1,
  update/2,
  delete/1,
  find_by_name/1,
  find_by_id/1,
  find_all_by_id/1,
  find_all_by_name/1,
  find_all_by_host/1,
  find_all_grouped_by_host/0,
  all/0
]).

-export ([
  new/1,
  is_same_as/2
]).

% APPLICATION STUFF
-export ([
  meta_data/2,
  build_app_env/1, build_app_env/2
]).

-define (DB, beehive_db_srv).

-export ([validate_bee/1]).

% META DATA
meta_data(FileLocation, MetaFile) ->
  BeeSize = case file:read_file_info(FileLocation) of
    {ok, FileInfo} -> FileInfo#file_info.size;
    _E -> 0.0
  end,
  OtherProps = case filelib:is_file(MetaFile) of
    true -> {ok, MetaProps} = file:consult(MetaFile), MetaProps;
    false -> []
  end,
  lists:flatten([{bee_size, BeeSize}, OtherProps]).

% Create a new bee
create(A) -> save(validate_bee(new(A))).

% Save the bee
save(Bee) when is_record(Bee, bee) ->
  case catch ?DB:write(bee, Bee#bee.id, Bee) of
    ok -> {ok, Bee};
    {'EXIT',{aborted,{no_exists,_}}} -> 
      ?NOTIFY({db, database_not_initialized, bee}),
      {error, database_not_initialized};
    E ->
      erlang:display({error, saving_Bee, Bee, E}),
      {error, did_not_write}
  end;
save([]) -> invalid;
save(Proplists) when is_list(Proplists) -> 
  case from_proplists(Proplists) of
    {error, _} = T -> T;
    Bee -> 
      save(Bee)
  end;
save(Func) when is_function(Func) ->
  ?DB:save(Func);
  
save(Else) -> {error, {cannot_save, Else}}.

new([]) -> error;
new(Bee) when is_record(Bee, bee) -> validate_bee(Bee);
new(Proplist) when is_list(Proplist) -> 
  validate_bee(from_proplists(Proplist));
new(Else) -> {error, {cannot_make_new_bee, Else}}.

read(Name) ->
  case find_by_name(Name) of
    Bee when is_record(Bee, bee) -> Bee;
    _E -> {error, not_found}
  end.

delete(Bee) when is_record(Bee, bee) ->
  ?DB:delete(bee, Bee#bee.id);
delete(Proplist) when is_list(Proplist) -> ?DB:delete(bee, Proplist);
delete(Name) when is_list(Name) -> ?DB:delete(bee, Name);
delete([]) -> invalid;
delete(Else) -> {error, {cannot_delete, Else}}.

all() -> ?DB:all(bee).

find_by_name(Name) ->
  case find_all_by_name(Name) of
    [H|_Rest] -> H;
    [] -> not_found;
    % This should ALWAYS be not_found, but just to be safe
    E -> E
  end.

find_by_id(Id) ->
  case find_all_by_id(Id) of
    [] -> not_found;
    [H|_Rest] -> H;
    E -> E
  end.

find_all_by_id(Id) ->
  case ?DB:read(bee, Id) of
    Bees when is_list(Bees) -> Bees;
    _ -> not_found
  end.

% Find alls
find_all_by_name(Name) -> ?DB:match(#bee{app_name = Name, _='_'}).
find_all_by_host(Host) -> ?DB:match(#bee{host = Host, _='_'}).
find_all_grouped_by_host() ->
  Q = qlc:keysort(#bee.host, db:table(bee)),
  db:transaction(
    fun() -> qlc:fold(fun find_all_grouped_by_host1/2, [], Q) end
  ).

find_all_grouped_by_host1(#bee{host=Host} = B, [{Host, Backends, Sum} | Acc]) ->
  [{Host, [B|Backends], Sum + 1} | Acc];
find_all_grouped_by_host1(#bee{host=Host} = B, Acc) ->
  [{Host, [B], 1}|Acc].
  
update([], _) -> ok;
update(Bee, NewProps) when is_record(Bee, bee) ->
  NewBee = misc_utils:update_proplist(to_proplist(Bee), NewProps),
  {ok, NewBee1} = save(NewBee),
  {updated, NewBee1};
update(Name, NewProps) ->
  Bee = find_by_name(Name),
  update(Bee, NewProps).

% There has got to be a better way?
is_same_as(Bee, Otherbee) ->
  Bee#bee.id == Otherbee#bee.id.

%%-------------------------------------------------------------------
%% @spec (Bee:app()) ->    {ok, Value}
%% @doc Build environment variables for the application
%%      
%% @end
%%-------------------------------------------------------------------
build_app_env(Bee) ->
  case apps:find_by_name(Bee#bee.app_name) of
    App when is_record(App, app) -> build_app_env(Bee, App);
    _ -> build_app_env(Bee, no_app)
  end.
build_app_env(#bee{app_name = AppName} = Bee, no_app) ->
  build_app_env(Bee, #app{name = AppName});
                
build_app_env(  #bee{ port        = Port, 
                      host        = HostIp, 
                      app_name    = AppName,
                      commit_hash = Sha,
                      start_time  = StartedAt
                    } = _Bee, App) -> 
  ScratchDisk = config:search_for_application_value(scratch_dir, ?BEEHIVE_DIR("tmp")),
  RunningDisk = config:search_for_application_value(scratch_dir, ?BEEHIVE_DIR("run")),
  LogDisk     = config:search_for_application_value(log_path, ?BEEHIVE_DIR("application_logs")),

  WorkingDir = filename:join([ScratchDisk, AppName]),
  RunningDir = filename:join([RunningDisk, AppName]),
  LogDir     = filename:join([LogDisk, AppName]),
  
  OtherOpts = [
    {name, AppName},
    {host_ip, HostIp},
    {sha, Sha},
    {port, misc_utils:to_list(Port)},
    {start_time, misc_utils:to_list(StartedAt)},
    {log_directory, LogDir},
    {working_directory, WorkingDir},
    {run_dir, RunningDir}
  ],
  
  EnvOpts = apps:build_app_env(App, OtherOpts),
  lists:map(fun(Dir) -> 
    file:make_dir(Dir) 
  end, [ScratchDisk, WorkingDir, RunningDisk, RunningDir, LogDisk, LogDir]),
  Opts = lists:flatten([{cd, RunningDir}, EnvOpts]),
  {ok, App, Opts}.

% If erlang had 'meta-programming,' we wouldn't have to do all this work to validate the proplists
from_proplists(Proplists) -> from_proplists(Proplists, #bee{}).
from_proplists([], Bee)  -> Bee;
from_proplists([{id, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{id = V});
from_proplists([{app_name, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{app_name = V});
from_proplists([{host, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{host = V});
from_proplists([{host_node, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{host_node = V});
from_proplists([{port, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{port = V});
from_proplists([{path, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{path = V});
from_proplists([{meta_data, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{meta_data = V});
% Do we need temp_name anymore?
from_proplists([{temp_name, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{temp_name = V});
from_proplists([{commit_hash, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{commit_hash = V});
from_proplists([{bee_size, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{bee_size = V});
from_proplists([{start_time, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{start_time = V});
from_proplists([{pid, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{pid = V});
from_proplists([{os_pid, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{os_pid = V});
from_proplists([{sticky, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{sticky = V});
from_proplists([{lastresp_time, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{lastresp_time = V});
from_proplists([{lasterr, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{lasterr = V});
from_proplists([{lasterr_time, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{lasterr_time = V});
from_proplists([{act_time, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{act_time = V});
from_proplists([{status, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{status = V});
from_proplists([{maxconn, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{maxconn = V});
from_proplists([{act_count, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{act_count = V});
% Prolly should remove this storage_node
from_proplists([{storage_node, V}|Rest], Bee) -> from_proplists(Rest, Bee#bee{storage_node = V});
from_proplists([_Other|Rest], Bee) -> from_proplists(Rest, Bee).

to_proplist(Bee) -> to_proplist(record_info(fields, bee), Bee, []).
to_proplist([], _Bee, Acc) -> Acc;
to_proplist([id|Rest], #bee{id = Id} = Bee, Acc) -> to_proplist(Rest, Bee, [{id, Id}|Acc]);
to_proplist([app_name|Rest], #bee{app_name = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{app_name, Value}|Acc]);
to_proplist([host|Rest], #bee{host = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{host, Value}|Acc]);
to_proplist([host_node|Rest], #bee{host_node = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{host_node, Value}|Acc]);
to_proplist([port|Rest], #bee{port = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{port, Value}|Acc]);
to_proplist([path|Rest], #bee{path = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{path, Value}|Acc]);
to_proplist([meta_data|Rest], #bee{meta_data = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{meta_data, Value}|Acc]);
to_proplist([temp_name|Rest], #bee{temp_name = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{temp_name, Value}|Acc]);
to_proplist([commit_hash|Rest], #bee{commit_hash = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{commit_hash, Value}|Acc]);
to_proplist([bee_size|Rest], #bee{bee_size = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{bee_size, Value}|Acc]);
to_proplist([start_time|Rest], #bee{start_time = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{start_time, Value}|Acc]);
to_proplist([pid|Rest], #bee{pid = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{pid, Value}|Acc]);
to_proplist([os_pid|Rest], #bee{os_pid = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{os_pid, Value}|Acc]);
to_proplist([sticky|Rest], #bee{sticky = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{sticky, Value}|Acc]);
to_proplist([lastresp_time|Rest], #bee{lastresp_time = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{lastresp_time, Value}|Acc]);
to_proplist([lasterr|Rest], #bee{lasterr = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{lasterr, Value}|Acc]);
to_proplist([lasterr_time|Rest], #bee{lasterr_time = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{lasterr_time, Value}|Acc]);
to_proplist([act_time|Rest], #bee{act_time = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{act_time, Value}|Acc]);
to_proplist([status|Rest], #bee{status = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{status, Value}|Acc]);
to_proplist([maxconn|Rest], #bee{maxconn = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{maxconn, Value}|Acc]);
to_proplist([act_count|Rest], #bee{act_count = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{act_count, Value}|Acc]);
to_proplist([storage_node|Rest], #bee{storage_node = Value} = Bee, Acc) -> to_proplist(Rest, Bee, [{storage_node, Value}|Acc]);
to_proplist([_H|T], Bee, Acc) -> to_proplist(T, Bee, Acc).

%%-------------------------------------------------------------------
%% @spec (Proplist) ->    ValidProplist
%% @doc Validate the proplist to create a new bee record
%%      
%% @end
%%-------------------------------------------------------------------
validate_bee(Bee) when is_record(Bee, bee) -> validate_bee(record_info(fields, bee), Bee).
validate_bee([], Bee) ->  Bee;
% Validate the id
validate_bee([id|Rest], #bee{id = undefined, app_name = AppName, host = Host, port = Port} = Bee) -> 
  validate_bee(Rest, Bee#bee{id = {AppName, Host, Port}});
validate_bee([id|Rest], Bee) -> 
  validate_bee(Rest, Bee);
% Validate the app_name
validate_bee([app_name|_Rest], #bee{app_name = undefined} = _Bee) -> {error, bee_not_associated_with_an_application};
validate_bee([app_name|Rest], Bee) -> validate_bee(Rest, Bee);
% Validate sticky-ness
validate_bee([sticky|Rest], #bee{sticky = false} = Bee) -> validate_bee(Rest, Bee);
validate_bee([sticky|Rest], #bee{sticky = true} = Bee) -> validate_bee(Rest, Bee);
validate_bee([sticky|_Rest], _Bee) -> {error, invalid_sticky_value};

% Validate others?
validate_bee([_H|Rest], Bee) -> validate_bee(Rest, Bee).
