#include "ei_decode.h"
#include "process_manager.h"

int decode_command_call_into_process(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], process_t **ptr)
{
  // Instantiate a new process
  if (pm_new_process(ptr))
    return -1;
  
  process_t *process = *ptr;
  const ERL_NIF_TERM* big_tuple;
  int arity = 2;
  // Get the outer tuple
  if(!enif_get_tuple(env, argv[0], &arity, &big_tuple)) return -1;
  
  // The first command is a string
  char command[MAX_BUFFER_SZ], key[MAX_BUFFER_SZ], value[MAX_BUFFER_SZ];
  memset(&command, '\0', sizeof(command));
  
  // Get the command
  if (enif_get_string(env, big_tuple[0], command, sizeof(command), ERL_NIF_LATIN1) < 0) return -1;
  pm_malloc_and_set_attribute(&process->command, command);
  
  // The second element of the tuple is a list of options
  const ERL_NIF_TERM* tuple;
  ERL_NIF_TERM head, tail, list = big_tuple[1];
  
  // int enif_get_tuple(ErlNifEnv* env, ERL_NIF_TERM term, int* arity, const ERL_NIF_TERM** array)
  while(enif_get_list_cell(env, list, &head, &tail)) {
    // Get the tuple
    if(!enif_get_tuple(env, head, &arity, &tuple)) return -1;
    // First element is an atom
    if (!enif_get_atom(env, tuple[0], key, sizeof(key))) return -2;
    if (enif_get_string(env, tuple[1], value, sizeof(value), ERL_NIF_LATIN1) < 0) return -3;
    if (!strcmp(key, "do_before")) {
      // Do before
      pm_malloc_and_set_attribute(&process->before, value);
    } else if (!strcmp(key, "do_after")) {
      pm_malloc_and_set_attribute(&process->after, value);
    } else if (!strcmp(key, "cd")) {
      pm_malloc_and_set_attribute(&process->cd, value);
    } else if (!strcmp(key, "env")) {
      pm_add_env(&process, value);
    } else if (!strcmp(key, "nice")) {
      process->nice = atoi(value);
    }
    list = tail;
  }
  return 0;
}

//---- Decoders ----//
char read_buf[BUFFER_SZ];
int  read_index = 0;

void nif_list_to_string(ErlNifEnv *env, ERL_NIF_TERM list, char *string)
{
  ERL_NIF_TERM head, tail;
  int character;

  while(enif_get_list_cell(env, list, &head, &tail)) {
    if(!enif_get_int(env, head, &character)) {
      return;
    }
    
    *string++ = (char)character;
    list = tail;
  };

  *string = '\0';
};

char *nif_arg_list_to_string(ErlNifEnv *env, ERL_NIF_TERM list, int *arg_size)
{
  ERL_NIF_TERM head, tail;
  char str_length[PREFIX_LEN], *args;
  int i, length, character;

  for(i=0; i<PREFIX_LEN; i++) {
    if(enif_get_list_cell(env, list, &head, &tail)) {
      if(!enif_get_int(env, head, &character)) {
        return NULL;
      }
      str_length[i] = (char)character;
      list = tail;
    } else {
      return NULL;
    }
  };

  length = atoi(str_length)+1;
  args = (char *)calloc(length, sizeof(char));

  nif_list_to_string(env, list, args);
  *arg_size = length;

  return args;
};

/**
* ok
**/
ERL_NIF_TERM ok(ErlNifEnv* env, const char* atom, const char *fmt, ...)
{
  char str[MAXATOMLEN];
  va_list vargs;
  va_start (vargs, fmt);
  vsnprintf(str, sizeof(str), fmt, vargs);
  va_end   (vargs);
  
  return enif_make_tuple2(env, enif_make_atom(env,atom), enif_make_atom(env, str));
}

/**
* error
**/
ERL_NIF_TERM error(ErlNifEnv* env, const char *fmt, ...)
{
  char str[MAXATOMLEN];
  va_list vargs;
  va_start (vargs, fmt);
  vsnprintf(str, sizeof(str), fmt, vargs);
  va_end   (vargs);
  
  return enif_make_tuple2(env, enif_make_atom(env,"error"), enif_make_atom(env, str));
}
