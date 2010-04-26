#ifndef BABYSITTER_EXEC_H
#define BABYSITTER_EXEC_H

#include <erl_nif.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <unistd.h>

#include "ei_decode.h"

/* Erlang exports */
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv);

/* Babysitter exports */
static ERL_NIF_TERM test_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM run_and_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* Test exports */
static ERL_NIF_TERM test_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif