#include "pm_helpers.h"

int string_index(const char* cmds[], const char *cmd)
{
  int i = 0;
  for (i = 0; *cmds != NULL; i++, cmds++) {
    if (!strncmp(cmd, *cmds, strlen(cmd))) return i;
  }
  return -1;
}

int pm_abs_path(const char *path)
{
  if ((path[0] == '/') || ((path[0] == '.') && (path[1] == '/')))
    return 0;
  else
    return -1;
}

char* str_chomp(const char *string)
{
  char *s, *t, *res;
  int len = strlen(string);
  res = (char*)malloc(sizeof(char*) * len);
  
  res = strdup(string);
  for (s = res; isspace (*s); s++) ;
  
  if (*s == 0) return (s);
  
  t = s + strlen (s) - 1;
  while (t > s && isspace(*t)) t--;
  *++t = '\0';
  return s;
}

char* str_safe_quote(const char *str)
{
  int len = 0;
  char *s, *res;
  len = strlen(str);
  
  res = (char*)malloc(sizeof(char*) * len);
  res = strdup(str);
  for (s = res; *s != (char)'\0'; s++)
    if (*s == '"') *s = '\"';
  
  return res;
}

const char *find_binary(const char *file)
{
  char buf[BUFFER_SZ];
  const char *p, *path;
  int len = 0, lp;
  
  if (file[0] == '\0') {
    errno = ENOENT;
    return NULL;
  }
  if (!strcmp(file,"")) return "";
  if (!pm_abs_path(file)) return file;
  
  // Get the path
  if (!(path = getenv("PATH"))) path = _PATH_DEFPATH;
	
  len = strlen(file);
  do {
    /* Find the end of this path element. */
		for (p = path; *path != 0 && *path != ':'; path++)
			continue;
		
		// If the path has a '.' at it, then it's local
		if (p == path) {
			p = ".";
			lp = 1;
		} else lp = path - p;
		    
    (void*)memset(buf, '\0', BUFFER_SZ);
    memcpy(buf, p, lp);
		buf[lp] = '/';
		memcpy(buf + lp + 1, file, len);
		buf[lp + len + 1] = '\0';
		
    if (0 == access(buf, X_OK)) {return strdup(buf);}
    
  } while(*path++ == ':');
  
  return file;
}


#define SKIP(p) while (*p && isspace (*p)) p++
#define WANT(p) *p && !isspace (*p)

/* Count the number of arguments. */
int count_args (const char * input)
{
  const char * p;
  int argc = 0;
  p = input;
  while (*p) {
    SKIP (p);
    if (WANT (p)) {
      argc++;
      while (WANT (p)) p++;
      }
  }
  return argc;
}
 
/* Copy each non-whitespace argument into its own allocated space. */
 
int copy_args (const char * input, int argc, char ** argv)
{
  int i = 0;
  const char *p;
  p = input;
  while (*p) {
    SKIP (p);
    if (WANT (p)) {
      const char* end = p;
      char* copy;
      while (WANT (end)) end++;
      copy = argv[i] = (char *)malloc (end - p + 1);
      if (! argv[i]) return -1;
      while (WANT (p)) *copy++ = *p++;
      *copy = 0;
      i++;
    }
  }
  if (i != argc) return -1;
  return 0;
}
 
int argify(const char *line, char ***argv_ptr)
{
  int argc;
  char ** argv;

  argc = count_args (line);
  if (argc == 0)
      return -1;
  argv = (char **)malloc (sizeof (char *) * argc);
  if (!argv) return -1;
  if (copy_args (line, argc, argv) < 0) return -1;
  *argv_ptr = argv;
  
  return argc;
}

#undef SKIP
#undef WANT
