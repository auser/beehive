#include <stdio.h>
#include "minunit.h"
// Tests
#include "test_helper.h"
#include "ei_decode_test.h"
#include "process_manager_test.h"
#include "pm_helpers_test.h"

static char * all_tests() {
  mu_run_test(test_new_process);
  mu_run_test(test_pm_check_pid_status);
  mu_run_test(test_pm_malloc_and_set_attribute);
  mu_run_test(test_pm_add_env);
  mu_run_test(test_pm_valid_process);
  mu_run_test(test_pm_abs_path);
  mu_run_test(test_find_binary);
  mu_run_test(test_string_index);
  mu_run_test(test_starting_a_process);
  mu_run_test(test_killing_a_process);
  mu_run_test(test_chomp_stringing);
  mu_run_test(test_running_a_process_as_a_script);
  return 0;
}

int main(int argc, char **argv) {
  char *result = all_tests();
  if (result != 0) {
    printf("[error] %s\n", result);
  }
  else {
    printf("ALL TESTS PASSED\n");
  }
  printf("Assertions made: %d\n", assertions_made);
  printf("Tests failed: %d\n", tests_failed);
  printf("Tests run: %d\n", tests_run);
  
  return result != 0;
}
