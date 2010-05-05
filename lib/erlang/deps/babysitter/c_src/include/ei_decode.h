#ifndef EI_DECODE_H
#define EI_DECODE_H

#include <ei.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#include "process_manager.h"
#include "pm_helpers.h"

// Defines

// Ei
enum BabysitterActionT {BS_RUN,BS_EXEC,BS_LIST,BS_STATUS,BS_KILL};
enum BabysitterActionT ei_decode_command_call_into_process(char *buf, process_t **ptr);
int decode_atom_index(char* buf, int *index, const char* cmds[]);

// Ei responses
int ei_pid_ok(int fd, int transId, pid_t pid);
int ei_pid_status_term(int fd, int transId, pid_t pid, int status);
int ei_send_pid_list(int fd, int transId, process_struct *hd, int size);
int ei_pid_status(int fd, int transId, pid_t pid, int status);
// Responses
int ei_error(int fd, int transId, const char* fmt, ...);
int ei_ok(int fd, int transId, const char* fmt, ...);

int ei_read(int fd, unsigned char** bufr);
int write_cmd(int fd, ei_x_buff *buff);
int read_exact(int fd, unsigned char *buf, int len);
int write_exact(int fd, unsigned char *buf, int len);

#endif
