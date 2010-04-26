#ifndef PROCESS_MANAGER_H
#define PROCESS_MANAGER_H

#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <limits.h>
#include <signal.h>
#include <sys/time.h>             // For timeval struct
#include <sys/wait.h>             // For waitpid
#include <time.h>                 // For time function

#include <setjmp.h>
#include "uthash.h"
#include "pm_helpers.h"

#include "print_helpers.h"

/* Types */
typedef struct _process_t_ {
  char**  env;
  int     env_c;
  int     env_capacity;
  char*   command;
  char*   before;
  char*   after;
  char*   cd;
  int     nice;
  pid_t   pid;            // Used only when kill is the action
  int     transId;        // Communication id
} process_t;

typedef struct _process_struct_ {
    pid_t pid;                  // key
    pid_t kill_pid;             // Kill pid
    time_t deadline;            // Deadline to kill the pid
    int status;                 // Status of the pid
    int transId;                // id of the transmission
    UT_hash_handle hh;          // makes this structure hashable
} process_struct;

/* Helpers */
int pm_new_process(process_t **ptr);

/* External exports */
int pm_check_pid_status(pid_t pid);
int pm_add_env(process_t **ptr, char *str);
int pm_process_valid(process_t **ptr);
int pm_free_process(process_t *p);
int pm_malloc_and_set_attribute(char **ptr, char *value);
void pm_set_can_jump();
void pm_set_can_not_jump();

/* extra helpers */
int pm_setup(int read_handle, int write_handle);

/* Mainly private exports */
pid_t pm_run_and_spawn_process(process_t *process);
pid_t pm_run_process(process_t *process);
int pm_kill_process(process_t *process);

pid_t pm_execute(int wait, const char* command, const char *cd, int nice, const char** env);
int pm_check_children(void (*child_changed_status)(process_struct *ps), int isTerminated);
int pm_check_pending_processes();
int pm_next_loop(void (*child_changed_status)(process_struct *ps));

#endif