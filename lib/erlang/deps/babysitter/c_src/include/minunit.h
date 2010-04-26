/* file: minunit.h */
#ifndef MINUNIT_H
#define MINUNIT_H

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#define mu_assert(test, message) do { assertions_made++;if (!(test)) {tests_failed++;return message;}} while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; if (message) return message; } while (0)
extern int tests_run;
extern int assertions_made;

#endif