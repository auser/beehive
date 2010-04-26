#include <stdio.h>
#include <string.h>
#include "pm_helpers.h"
#include "minunit.h"
#include "test_helper.h"

char *test_pm_abs_path() {
  mu_assert(pm_abs_path("./bin/path") == 0, "./bin/path is not an absolute path");
  mu_assert(pm_abs_path("/bin/bash") == 0, "/bin/bash is not an absolute path");
  mu_assert(pm_abs_path("binary") == -1, "binary is an absolute path");
  mu_assert(pm_abs_path(" ./binary") == -1, " ./binary is an absolute path");
  return 0;
}

char *test_find_binary() {
  mu_assert(!strcmp(find_binary("/bin/bash"), "/bin/bash"), "/bin/bash was not an absolute path and could not be found");
  mu_assert(!strcmp(find_binary("bash"), "/bin/bash"), "/bin/bash was not an absolute path and could not be found");
  mu_assert(!strcmp(find_binary("made_up_binary"), "\0"), "made_up_binary was found?!? ");
  return 0;
}

char *test_string_index() {
  const char* options[] = {"cd", "env", "kill", "nice", "user", "stdout", "stderr", NULL};
  
  mu_assert(string_index(options, "env") == 1, "string_index returns incorrect index for 'env'");
  mu_assert(string_index(options, "cd") == 0, "string_index returns incorrect index for 'cd'");
  mu_assert(string_index(options, "user") == 4, "string_index returns incorrect index for 'user'");
  mu_assert(string_index(options, "dogs") == -1, "string_index returns incorrect index for 'dogs'");
  return 0;
}

char *test_chomp_stringing() {
  mu_assert(!strcmp(str_chomp(" hello world "), "hello world"), "str_chomp is bunk");
  mu_assert(!strcmp(str_chomp("\nhello world "), "hello world"), "str_chomp is bunk");
  mu_assert(!strcmp(str_chomp("\r\n\thello world\n "), "hello world"), "str_chomp is bunk");
  return 0;
}