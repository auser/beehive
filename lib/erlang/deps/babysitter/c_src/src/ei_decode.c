#include "ei_decode.h"
#include "process_manager.h"

/**
* Decode the arguments into a new_process
*
* @params
* {Cmd::string(), [Option]}
*     Option = {env, Strings} | {cd, Dir} | {do_before, Cmd} | {do_after, Cmd} | {nice, int()}
**/
const char* babysitter_action_strings[] = {"run", "exec", "list", "status", "kill", NULL};
enum BabysitterActionT ei_decode_command_call_into_process(char *buf, process_t **ptr)
{
  int err_code = -1;
  // Instantiate a new process
  if (pm_new_process(ptr)) return err_code--;
  
  int   arity, index, version, size;
  int i = 0, tuple_size, type;
  long  transId;
    
  // Reset the index, so that ei functions can decode terms from the 
  // beginning of the buffer
  index = 0;

  /* Ensure that we are receiving the binary term by reading and 
   * stripping the version byte */
  if (ei_decode_version(buf, &index, &version) < 0) return err_code--;
  // Decode the tuple header and make sure that the arity is 2
  // as the tuple spec requires it to contain a tuple: {TransId, {Cmd::atom(), Arg1, Arg2, ...}}
  if (ei_decode_tuple_header(buf, &index, &arity) < 0) return err_code--;; // decode the tuple and capture the arity
  if (ei_decode_long(buf, &index, &transId) < 0) return err_code--;; // Get the transId
  if ((ei_decode_tuple_header(buf, &index, &arity)) < 0) return err_code--;; 
  
  process_t *process = *ptr;
  process->transId = transId;
  
  // Get the outer tuple
  // The first command is an atom
  // {Cmd::atom(), Command::string(), Options::list()}
  ei_get_type(buf, &index, &type, &size); 
  char *action = NULL; if ((action = (char*) calloc(sizeof(char*), size + 1)) == NULL) return err_code--;
  // Get the command
  if (ei_decode_atom(buf, &index, action)) return err_code--;
  int ret = -1;
  if ((int)(ret = (enum BabysitterActionT)string_index(babysitter_action_strings, action)) < 0) return err_code--;
  
  switch(ret) {
    case BS_STATUS:
    case BS_KILL: {
      ei_get_type(buf, &index, &type, &size);
      long lval;
      ei_decode_long(buf, &index, &lval);
      process->pid = (pid_t)lval;
    }
    break;
    case BS_LIST:
    break;
    default:
      // Get the next string
      ei_get_type(buf, &index, &type, &size); 
      char *command = NULL; if ((command = (char*) calloc(sizeof(char*), size + 1)) == NULL) return err_code--;

      // Get the command  
      if (ei_decode_string(buf, &index, command) < 0) return err_code--;
      pm_malloc_and_set_attribute(&process->command, command);

      // The second element of the tuple is a list of options
      if (ei_decode_list_header(buf, &index, &size) < 0) return err_code--;

      enum OptionT            { CD,   ENV,   NICE,  DO_BEFORE, DO_AFTER,     STDOUT,    STDERR } opt;
      const char* options[] = {"cd", "env", "nice", "do_before", "do_after", "stdout", "stderr", NULL};

      for (i = 0; i < size; i++) {
        // Decode the tuple of the form {atom, string()|int()};
        if (ei_decode_tuple_header(buf, &index, &tuple_size) < 0) return err_code--;

        if ((int)(opt = (enum OptionT)decode_atom_index(buf, &index, options)) < 0) return err_code--;

        switch (opt) {
          case CD:
          case DO_BEFORE:
          case DO_AFTER:
          case ENV: {
            int size;
            ei_get_type(buf, &index, &type, &size); 
            char *value = NULL;
            if ((value = (char*) calloc(sizeof(char*), size + 1)) == NULL) return err_code--;

            if (ei_decode_string(buf, &index, value) < 0) {
              fprintf(stderr, "ei_decode_string error: %d\n", errno);
              free(value);
              return err_code--;
            }
            if (opt == CD)
              pm_malloc_and_set_attribute(&process->cd, value);
            else if (opt == ENV)
              pm_add_env(&process, value);
            else if (opt == DO_BEFORE)
              pm_malloc_and_set_attribute(&process->before, value);
            else if (opt == DO_AFTER)
              pm_malloc_and_set_attribute(&process->after, value);

            free(value);
          }
          break;
          case NICE: {
            long lval;
            ei_decode_long(buf, &index, &lval);
            process->nice = lval;
          }
          break;
          case STDOUT:
          case STDERR: {
            // Put this somewhere
          }
          break;
          default:
            return err_code--;
          break;
        }
      }
    break;
  }
  *ptr = process;
  return ret;
}

/**
* Basic read off the buffer. Extract the length from the header
**/
int read_cmd(int fd, unsigned char **bufr, int *size)
{
  unsigned char *buf = *bufr;
  
  int header_len = 2;
  if ((read_exact(fd, buf, header_len)) != header_len) return -1;
  
  int len = 0;
  int i = 0;
  for(i = 0; i < header_len; i++) len |= buf[i] << (8*(header_len-i-1));
  
  if (len > *size) {
    unsigned char* tmp = (unsigned char *) realloc(buf, len);
    if (tmp == NULL)
      return -1;
    else
      buf = tmp;
    *size = len;
  }
  int ret = read_exact(fd, buf, len);
  *bufr = buf;
  return ret;
}

int ei_read(int fd, unsigned char** bufr)
{
  int size = MAX_BUFFER_SZ;
  unsigned char *buf;
  if ((buf = (unsigned char *) malloc(sizeof(unsigned char) * size)) == NULL) {
    fprintf(stderr, "Could not create an erlang buffer: %s\n", strerror(errno));
    return -1;
  }
  
  int ret = read_cmd(fd, &buf, &size);
  
  if (ret > 0) *bufr = buf;
  return ret;
}

/**
* Data marshalling functions
**/

int encode_header(ei_x_buff *result, int transId, int next_tuple_len)
{
  if (ei_x_new_with_version(result)) return -1;
  if (ei_x_encode_tuple_header(result, 2)) return -1;
  if (ei_x_encode_long(result, transId)) return -2;
  if (ei_x_encode_tuple_header(result, next_tuple_len)) return -2;
  return 0;
}

/**
* ei_write_atom
* @params
*   fd - File descriptor to write to
*   transId - TransID
*   first - the atom to send
*   fmt, ... - The string to write out
* @returns
*   {transId, {Atom::atom(), Result::string()}}
**/
int ei_write_atom(int fd, int transId, const char* first, const char* fmt, ...)
{
  ei_x_buff result;
  if (encode_header(&result, transId, 2)) return -1;
  if (ei_x_encode_atom(&result, first) < 0) return -3;
  // Encode string
  char str[MAX_BUFFER_SZ];
  
  va_list vargs;
  va_start(vargs, fmt);
  vsnprintf(str, MAX_BUFFER_SZ, fmt, vargs);
  va_end(vargs);
  if (ei_x_encode_string_len(&result, str, strlen(str))) return -4;
  
  write_cmd(fd, &result);
  ei_x_free(&result);
  return 0;
}

int ei_pid_ok(int fd, int transId, pid_t pid)
{
  ei_x_buff result;
  if (encode_header(&result, transId, 2)) return -1;
  if (ei_x_encode_atom(&result, "ok") ) return -3;
  // Encode pid
  if (ei_x_encode_long(&result, (int)pid)) return -5;
  if (write_cmd(fd, &result) < 0) return -5;
  ei_x_free(&result);
  return 0;
}

/**
* Send a list of pids
* {transId, [Pid::integer()]}
**/
int ei_send_pid_list(int fd, int transId, process_struct *hd, int size)
{
  ei_x_buff result;
  process_struct *ps;
  if (encode_header(&result, transId, 2)) return -1;
  if (ei_x_encode_atom(&result, "ok") ) return -2;
  if (ei_x_encode_list_header(&result, size)) return -3;
  for( ps = hd; ps != NULL; ps = ps->hh.next ) ei_x_encode_long(&result, ps->pid);
  if (ei_x_encode_empty_list(&result)) return -4;
  if (write_cmd(fd, &result) < 0) return -5;
  ei_x_free(&result);
  return 0;
}

int ei_pid_status_header(int fd, int transId, pid_t pid, int status, const char* header)
{
  ei_x_buff result;
  if (encode_header(&result, transId, 3)) return -1;
  if (ei_x_encode_atom(&result, header) ) return -4;
  // Encode pid
  if (ei_x_encode_long(&result, (int)pid)) return -5;
  if (ei_x_encode_long(&result, (int)status)) return -5;
  if (write_cmd(fd, &result) < 0) {
    return -5;
  }
  ei_x_free(&result);
  return 0;
}
int ei_pid_status(int fd, int transId, pid_t pid, int status)
{
  return ei_pid_status_header(fd, transId, pid, status, "status");
}

int ei_pid_status_term(int fd, int transId, pid_t pid, int status)
{
  return ei_pid_status_header(fd, transId, pid, status, "exit_status");
}

int ei_ok(int fd, int transId, const char* fmt, ...)
{  
  va_list *vargs = NULL;
  return ei_write_atom(fd, transId, "ok", fmt, *vargs);
}
int ei_error(int fd, int transId, const char* fmt, ...){
  va_list *vargs = NULL;
  return ei_write_atom(fd, transId, "error", fmt, *vargs);
}

int decode_atom_index(char* buf, int *index, const char* cmds[])
{
  int type, size;
  ei_get_type(buf, index, &type, &size); 
  char *atom_name = NULL;
  if ((atom_name = (char*) calloc(sizeof(char), size)) == NULL) return -1;
  
  if (ei_decode_atom(buf, index, atom_name)) return -1;
  
  int ret = string_index(cmds, atom_name);
  free(atom_name);
  return ret;
}

/**
* Data i/o
**/
int write_cmd(int fd, ei_x_buff *buff)
{
  unsigned char li;

  li = (buff->index >> 8) & 0xff;
  write_exact(fd, &li, 1);
  li = buff->index & 0xff;
  write_exact(fd, &li, 1);
  return write_exact(fd, (unsigned char*)buff->buff, buff->index);
}

int read_exact(int fd, unsigned char *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(fd, buf+got, len-got)) <= 0)
      return i;
    got += i;
  } while (got<len);

  return len;
}

int write_exact(int fd, unsigned char *buf, int len)
{
  int i, wrote = 0;
  
  do {
    if ((i = write(fd, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  return len;
}
