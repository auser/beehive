#include "print_helpers.h"


int debug(const long int cur_level, int level, const char *fmt, ...)
{
  int r;
  va_list ap;
  if (cur_level < level) return 0;
	va_start(ap, fmt);
  fprintf(stderr, "[debug %d] ", level);
	r = vfprintf(stderr, fmt, ap);
	va_end(ap);
	return r;
}