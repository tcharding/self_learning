#include "tch.h"
#include <stdarg.h>

/* msgn: write fmt string to stderr */
void msg(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

/* msgn: write fmt string to stderr, new line added */
void msgn(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

/* debug: write fmt string to stderr, if DEBUG 1 */
void debug(const char *fmt, ...)
{
#ifdef DEBUG
	va_list		ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
#endif
}

