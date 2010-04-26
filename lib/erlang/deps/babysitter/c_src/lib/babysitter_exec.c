#include "babysitter_exec.h"
#include "process_manager.h"
#include "ei_decode.h"
#include "nif_decode.h"

/**
* Test if the pid is up or not
*
* @params
*   {pid, Int}
* @return
*   up
*   down
**/
ERL_NIF_TERM test_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc != 1) return error(env, "Wrong argument signature");
  
  long pid;
  char atom[MAX_BUFFER_SZ];
  const ERL_NIF_TERM* tuple;
  int arity = 2;
  
  if(!enif_get_tuple(env, argv[0], &arity, &tuple)) return -1;
  
  memset(&atom, '\0', sizeof(atom));
  if (!enif_get_atom(env, tuple[0], atom, sizeof(atom)) < 0) return -1;
  
  if (!strncmp(atom, "pid", 3)) {
    enif_get_long(env, tuple[1], &pid);
    if (pid < 1)
      return error(env, "Invalid pid: %d", (int)pid);
    
    if (pm_check_pid_status(pid) == 0)
      return enif_make_atom(env, "up");
    else
      return enif_make_atom(env, "down");
  } else {
    return error(env, "Wrong signature");
  }
} 

/**
* Launch a new process
*
* @params
*   {Cmd::string(), Options:list()}
*     Options = {do_before, string()} | {do_after, string()} | {env, string()} | {cd, string()}
* @return
*   {pid, Pid:long()} | {error, Reason:string()}
**/
ERL_NIF_TERM run_and_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM erlRes;
  pid_t pid;
  process_t *process = NULL;
  decode_command_call_into_process(env, argc, argv, &process);
  // Do something with the process
  if (pm_process_valid(&process)) return error(env, "invalid process specification");
  
  pid = pm_run_and_spawn_process(process);
  if (pid > 0)
    erlRes = enif_make_tuple2(env, enif_make_atom(env,"pid"), enif_make_ulong(env, pid));
  else
    erlRes = error(env, "failure to launch");
  
  pm_free_process(process);
  return erlRes;
  
}

/**
* Test the arguments of the erlang call
*
* @params
*   {Cmd::string(), Options:list()}
*     Options = {do_before, string()} | {do_after, string()} | {env, string()} | {cd, string()}
* @return
*   {pid, Pid:long()} | {error, Reason:string()}
**/
ERL_NIF_TERM test_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM erlRes;
  process_t *process = NULL;
  decode_command_call_into_process(env, argc, argv, &process);
  // Do something with the process
  // erlRes = enif_make_atom(env, "ok");
  if (pm_process_valid(&process)) return error(env, "invalid process specification");
  pid_t pid = getpid(); // Dummy pid
  if (pid > 0)
    erlRes = enif_make_tuple2(env, enif_make_atom(env,"pid"), enif_make_ulong(env, pid));
  else
    erlRes = error(env, "failure to launch");
    
  pm_free_process(process);
  return erlRes;
}

static ErlNifFunc nif_funcs[] =
{
  {"test_pid", 1, test_pid},
  {"test_args", 1, test_args},
  {"run_and_monitor", 1, run_and_monitor}
};

ERL_NIF_INIT(babysitter_exec, nif_funcs, load, reload, upgrade, unload)

/*--- Erlang stuff ---*/
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
  return;
}
