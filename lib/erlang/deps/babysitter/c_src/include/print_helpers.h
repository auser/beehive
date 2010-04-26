#ifndef PRINT_HELPERS_H
#define PRINT_HELPERS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>

int debug(const long int cur_level, int level, const char *fmt, ...);

#endif