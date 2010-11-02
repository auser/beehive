-module (bees).

-include ("beehive.hrl").
-include ("common.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

%% DATABASE STUFF
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

%% APPLICATION STUFF
-export ([
          meta_data/2,
          build_app_env/1, build_app_env/2,
          from_bee_object/2
         ]).

-define (DB, beehive_db_srv).

-export ([validate_bee/1]).

%% META DATA
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

%% Create a new bee
create(Bee) ->
  case new(Bee) of
    NewBee when is_record(NewBee, bee) ->
      save(validate_bee(NewBee));
    E -> {error, E}
  end.

%% Save the bee
save(Bee) when is_record(Bee, bee) ->
  NewBee = validate_bee(Bee),
  case ?DB:write(bee, NewBee#bee.id, NewBee) of
    ok -> {ok, NewBee};
    {'EXIT',{aborted,{no_exists,_}}} ->
      ?NOTIFY({db, database_not_initialized, bee}),
      timer:sleep(100),
      {error, database_not_initialized};
    E ->
      %% TODO: Investigate why this EVER happens...
      {error, {did_not_write, E}}
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
new(Bee) when is_record(Bee, bee) -> Bee;
new(Proplist) when is_list(Proplist) ->
  from_proplists(Proplist);
new(Else) -> {error, {cannot_make_new_bee, Else}}.

read(Name) ->
  case find_by_name(Name) of
    Bee when is_record(Bee, bee) -> Bee;
    _E -> {error, not_found}
  end.

delete(Bee) when is_record(Bee, bee) ->
  ?DB:delete(bee, Bee#bee.id);
delete(Name) when is_list(Name) ->
  case bees:find_by_name(Name) of
    not_found -> invalid;
    Bee -> delete(Bee)
  end;
delete([]) -> invalid;
delete(Else) -> {error, {cannot_delete, Else}}.

all() -> ?DB:all(bee).

find_by_name(Name) ->
  case find_all_by_name(Name) of
    [H|_Rest] -> H;
    [] -> not_found;
    %% This should ALWAYS be not_found, but just to be safe
    E -> E
  end.

find_by_id(Id) ->
  case find_all_by_id(Id) of
    [] -> not_found;
    [H|_Rest] -> H;
    E -> E
  end.

%% Find alls
find_all_by_id(Id) -> ?DB:match(#bee{id = Id, _='_'}).
find_all_by_name(Name) -> ?DB:match(#bee{app_name = Name, _='_'}).
find_all_by_host(Host) -> ?DB:match(#bee{host = Host, _='_'}).
find_all_grouped_by_host() ->
  Q = qlc:keysort(#bee.host, bees:all()),
  qlc:fold(fun find_all_grouped_by_host1/2, [], Q).

find_all_grouped_by_host1(#bee{host=Host} = B, [{Host, Backends, Sum} | Acc]) ->
  [{Host, [B|Backends], Sum + 1} | Acc];
find_all_grouped_by_host1(#bee{host=Host} = B, Acc) ->
  [{Host, [B], 1}|Acc].

update([], _) -> ok;
update(Bee, OtherBee) when
    is_record(Bee, bee) andalso is_record(OtherBee, bee) ->
  update(Bee, lists:flatten(to_proplist(OtherBee)));
update(Bee, NewProps) when is_record(Bee, bee) ->
  NewBee = misc_utils:update_proplist(to_proplist(Bee), NewProps),
  {ok, NewBee1} = save(NewBee),
  {updated, NewBee1};
update(Name, NewProps) ->
  Bee = find_by_name(Name),
  update(Bee, NewProps).

%% There has got to be a better way?
is_same_as(Bee, Otherbee) ->
  Bee#bee.id == Otherbee#bee.id.

%%-------------------------------------------------------------------
%% @spec (Bee::bee()) ->    {ok, Value}
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
                      revision = Sha,
                      start_time  = StartedAt
                    } = _Bee, App) ->
  ScratchDisk = config:search_for_application_value(scratch_dir,
                                                    ?BEEHIVE_DIR("tmp")),
  RunningDisk = config:search_for_application_value(run_dir,
                                                    ?BEEHIVE_DIR("run")),
  LogDisk     =
    config:search_for_application_value(log_path,
                                        ?BEEHIVE_DIR("application_logs")),

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
               {run_directory, RunningDir}
              ],

  EnvOpts = apps:build_app_env(App, OtherOpts),
  lists:foreach(fun(Dir) -> file:make_dir(Dir) end,
                [ScratchDisk, WorkingDir, RunningDisk,
                 RunningDir, LogDisk, LogDir]),
  Opts = lists:flatten([{cd, RunningDir}, EnvOpts]),
  {ok, App, Opts}.

%% If erlang had 'meta-programming,' we wouldn't have to do all this
%% work to validate the proplists
from_proplists(Proplists) -> from_proplists(Proplists, #bee{}).
from_proplists([], Bee)  -> Bee;
from_proplists([{id, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{id = V});
from_proplists([{app_name, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{app_name = V});
from_proplists([{host, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{host = V});
from_proplists([{host_node, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{host_node = V});
from_proplists([{port, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{port = V});
from_proplists([{revision, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{revision = V});
from_proplists([{bee_size, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{bee_size = V});
from_proplists([{start_time, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{start_time = V});
from_proplists([{pid, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{pid = V});
from_proplists([{os_pid, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{os_pid = V});
from_proplists([{sticky, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{sticky = V});
from_proplists([{lastresp_time, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{lastresp_time = V});
from_proplists([{lasterr, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{lasterr = V});
from_proplists([{lasterr_time, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{lasterr_time = V});
from_proplists([{act_time, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{act_time = V});
from_proplists([{status, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{status = V});
from_proplists([{maxconn, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{maxconn = V});
from_proplists([{act_count, V}|Rest], Bee) ->
  from_proplists(Rest, Bee#bee{act_count = V});
from_proplists([_Other|Rest], Bee) -> from_proplists(Rest, Bee).

to_proplist(Bee) -> to_proplist(record_info(fields, bee), Bee, []).
to_proplist([], _Bee, Acc) -> Acc;
to_proplist([id|Rest], #bee{id = Id} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{id, Id}|Acc]);
to_proplist([app_name|Rest], #bee{app_name = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{app_name, Value}|Acc]);
to_proplist([host|Rest], #bee{host = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{host, Value}|Acc]);
to_proplist([host_node|Rest], #bee{host_node = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{host_node, Value}|Acc]);
to_proplist([port|Rest], #bee{port = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{port, Value}|Acc]);
to_proplist([revision|Rest], #bee{revision = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{revision, Value}|Acc]);
to_proplist([bee_size|Rest], #bee{bee_size = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{bee_size, Value}|Acc]);
to_proplist([start_time|Rest], #bee{start_time = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{start_time, Value}|Acc]);
to_proplist([pid|Rest], #bee{pid = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{pid, Value}|Acc]);
to_proplist([os_pid|Rest], #bee{os_pid = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{os_pid, Value}|Acc]);
to_proplist([sticky|Rest], #bee{sticky = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{sticky, Value}|Acc]);
to_proplist([lastresp_time|Rest], #bee{lastresp_time = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{lastresp_time, Value}|Acc]);
to_proplist([lasterr|Rest], #bee{lasterr = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{lasterr, Value}|Acc]);
to_proplist([lasterr_time|Rest], #bee{lasterr_time = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{lasterr_time, Value}|Acc]);
to_proplist([act_time|Rest], #bee{act_time = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{act_time, Value}|Acc]);
to_proplist([status|Rest], #bee{status = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{status, Value}|Acc]);
to_proplist([maxconn|Rest], #bee{maxconn = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{maxconn, Value}|Acc]);
to_proplist([act_count|Rest], #bee{act_count = Value} = Bee, Acc) ->
  to_proplist(Rest, Bee, [{act_count, Value}|Acc]);
to_proplist([_H|T], Bee, Acc) -> to_proplist(T, Bee, Acc).

from_bee_object(Bo, App) when is_record(Bo, bee_object) ->
  fbo(record_info(fields, bee_object), Bo, App, #bee{}).
fbo([], _Bo, _App, Bee) -> validate_bee(Bee);
fbo([name|Rest], #bee_object{name = Name} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{app_name = Name});
fbo([revision|Rest], #bee_object{revision = Rev} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{revision = Rev});
fbo([bee_size|Rest], #bee_object{bee_size = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{bee_size = V});
fbo([template|Rest], #bee_object{template = undefined} = Bo,
    #app{template = Type} = App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{template = Type});
fbo([template|Rest], #bee_object{template = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{template = V});
fbo([bee_file|Rest], #bee_object{bee_file = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{bee_file = V});
fbo([port|Rest], #bee_object{port = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{port = V});
fbo([host|Rest], #bee_object{port = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{port = V});
fbo([os_pid|Rest], #bee_object{os_pid = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{os_pid = V});
fbo([pid|Rest], #bee_object{pid = V} = Bo, App, Bee) ->
  fbo(Rest, Bo, App, Bee#bee{pid = V});
fbo([_H|Rest], Bo, App, Bee) -> fbo(Rest, Bo, App, Bee).

%%-------------------------------------------------------------------
%% @spec (Proplist) ->    ValidProplist
%% @doc Validate the proplist to create a new bee record
%%
%% @end
%%-------------------------------------------------------------------
validate_bee(Bee) when is_record(Bee, bee) ->
  ValidatedBee = validate_bee(lists:reverse(record_info(fields, bee)), Bee),
  ValidatedBee;
validate_bee(Else) -> Else.

validate_bee([], Bee) ->  Bee;
%% Validate the id
validate_bee([id|Rest],
             #bee{id = undefined, app_name = AppName,
                  host = Host, port = Port} = Bee) ->
  validate_bee(Rest, Bee#bee{id = {AppName, Host, Port}});
validate_bee([id|Rest], Bee) ->
  validate_bee(Rest, Bee);
%% Validate the app_name
validate_bee([app_name|_Rest], #bee{app_name = undefined} = _Bee) ->
  {error, bee_not_associated_with_an_application};
validate_bee([app_name|Rest], Bee) -> validate_bee(Rest, Bee);
%% Validate sticky-ness
validate_bee([sticky|Rest], #bee{sticky = false} = Bee) ->
  validate_bee(Rest, Bee);
validate_bee([sticky|Rest], #bee{sticky = true} = Bee) ->
  validate_bee(Rest, Bee);
validate_bee([sticky|_Rest], _Bee) ->
  {error, invalid_sticky_value};
%% Validate the host
validate_bee([host|Rest], #bee{host = undefined} = Bee) ->
  validate_bee(Rest, Bee#bee{host = bh_host:myip()});
%% Validate others?
validate_bee([_H|Rest], Bee) -> validate_bee(Rest, Bee).
