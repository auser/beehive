#ifndef NIF_DECODE_H
#define NIF_DECODE_H

#include <ei.h>
#include <erl_nif.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#include "process_manager.h"
#include "pm_helpers.h"

// Defines

/* Exports */
ERL_NIF_TERM ok(ErlNifEnv* env, const char* atom, const char *fmt, ...);
ERL_NIF_TERM error(ErlNifEnv* env, const char *fmt, ...);

// Decoders
int decode_command_call_into_process(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], process_t **ptr);
void nif_list_to_string(ErlNifEnv *env, ERL_NIF_TERM list, char *string);
char *nif_arg_list_to_string(ErlNifEnv *env, ERL_NIF_TERM list, int *arg_size);

#endif
