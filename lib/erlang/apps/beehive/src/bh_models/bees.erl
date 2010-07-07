%%%-------------------------------------------------------------------
%%% File    : bees.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov  6 13:15:07 PST 2009
%%%-------------------------------------------------------------------

-module (bees).
-include ("common.hrl").

% This provides an interface to the bee mnesia table that looks like:
% |----------|
% | bees |
% |----------|
% | name     |
% | host     |
% | ...      |
% |----------|
% 

-include ("beehive.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-export ([
  meta_data/2,
  find_by_name/1,
  find_all_by_name/1,
  find_all_by_host/1,
  find_by_id/1,
  find_all_grouped_by_host/0,
  create/1,
  update/1,
  delete/1, delete/3,
  all/0, 
  new/1,
  save/1,transactional_save/1,
  is_the_same_as/2,
  build_app_env/1, build_app_env/2
]).

%%-------------------------------------------------------------------
%% @spec (File::string()) ->    Proplist
%% @doc Take a bee location and find out as much meta-data as it can
%%      
%% @end
%%-------------------------------------------------------------------
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

find_by_name(Hostname) ->
  case find_all_by_name(Hostname) of
    [B|_] -> B;
    _ -> []
  end.
  
find_by_id(Id) ->
  case db:match(#bee{id = Id, _='_'}) of
    [B|_] -> B;
    _ -> []
  end.

find_all_by_name(Hostname) -> 
  db:match(#bee{app_name=Hostname, _='_'}).

find_all_by_host(Host) ->
  db:match(#bee{host=Host,_='_'}).
  
find_all_grouped_by_host() ->
  Q = qlc:keysort(#bee.host, db:table(bee)),
  db:transaction(
    fun() -> qlc:fold(fun find_all_grouped_by_host1/2, [], Q) end
  ).

find_all_grouped_by_host1(#bee{host=Host} = B, [{Host, Backends, Sum} | Acc]) ->
  [{Host, [B|Backends], Sum + 1} | Acc];
find_all_grouped_by_host1(#bee{host=Host} = B, Acc) ->
  [{Host, [B], 1}|Acc].

create(Backend) when is_record(Backend, bee) -> save(Backend);
create(NewProps) ->
  create(new(NewProps)).

save(Bee) ->
  case db:write(Bee) of
    ok -> {ok, Bee};
    {'EXIT',{aborted,{no_exists,_}}} -> 
      ?NOTIFY({db, database_not_initialized, bee}),
      {error, database_not_initialized};
    E ->
        io:format("Unknown error: ~p~n", [E]),
      {error, did_not_write}
  end.

% Grrr update!
transactional_save(F) ->
  db:transaction(F()).
  
update(Backend) -> 
  create(Backend).

delete(Backend) when is_record(Backend, bee) -> db:delete_object(Backend);
delete(Name) -> db:delete_object(#bee{app_name=Name, _='_'}).
delete(Name, Host, Port) ->
  db:delete_object(#bee{app_name=Name, host=Host, port=Port, _='_'}).

all() ->
  db:find(qlc:q([ B || B <- mnesia:table(bee) ])).

% There has got to be a better way?
is_the_same_as(Bee, Otherbee) ->
  Bee#bee.id == Otherbee#bee.id.

build_app_env(#bee{app_name = AppName} = Bee) ->
  case apps:find_by_name(AppName) of 
    [] -> {error, not_associated_with_an_app};
    App -> build_app_env(Bee, App)
  end.

build_app_env(  #bee{ port        = Port, 
                      host        = HostIp, 
                      app_name    = AppName,
                      commit_hash = Sha,
                      start_time  = StartedAt
                    } = _Bee, App) -> 
  ScratchDisk = config:search_for_application_value(scratch_disk, ?BEEHIVE_DIR("tmp")),
  RunningDisk = config:search_for_application_value(scratch_disk, ?BEEHIVE_DIR("run")),
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


new(NewProps) ->
  PropList = ?rec_info(bee, #bee{}),
  FilteredProplist1 = misc_utils:filter_proplist(PropList, NewProps, []),
  FilteredProplist2 = misc_utils:new_or_previous_value(FilteredProplist1, PropList, []),
  Id = make_id_from_proplists(NewProps),
  FilteredProplist3 = validate_bee_proplists(FilteredProplist2),
  FilteredProplist  = [{id, Id}|FilteredProplist3],
  list_to_tuple([bee|[proplists:get_value(X, FilteredProplist) || X <- record_info(fields, bee)]]).

% Make an id
make_id_from_proplists(PropList) ->
  Name = case proplists:get_value(name, PropList) of
    undefined -> proplists:get_value(app_name, PropList);
    E -> E
  end,
  Name = proplists:get_value(app_name, PropList, proplists:get_value(name, PropList)),
  Host = proplists:get_value(host, PropList),
  Port = proplists:get_value(port, PropList),
  {Name, Host, Port}.

validate_bee_proplists(PropList) ->
  lists:map(fun({Key, Val}) ->
    case Key of
      port -> {Key, misc_utils:to_integer(Val)};
      name -> {app_name, Val};
      meta_param -> {Key, Val};
      _ -> {Key, Val}
    end
  end, PropList).
