#include "tch.h"
#include <stdarg.h>

/*
 * C String Library
 */ 

/* s_dup: duplicate s using memory streams
    returned memory must be free'd, NULL on error with errno set */
char *s_dup(const char *s)
{
	FILE *stream;		
	size_t size;
	char *buf;
	int err;

	if (s == NULL)		/* normal condition */
		return NULL;

	buf = NULL;
	if ((stream = open_memstream(&buf, &size)) == NULL)
		return NULL;
	if (fputs(s, stream) < 0) {
		err = errno, free(buf), errno = err;
		return NULL;
	}
	if (fclose(stream) != 0) {
		err = errno, free(buf), errno = err;
		return NULL;
	}
	return buf;
}

/* s_dupfmt: duplicate fmt string using memory streams
    returned memory must be free'd, NULL on error with errno set */
char *s_dupfmt(const char *fmt, ...)
{
	FILE *stream;		
	size_t size;
	char *buf;
	int err;
	va_list ap;

	va_start(ap, fmt);

	buf = NULL;
	if ((stream = open_memstream(&buf, &size)) == NULL)
		return NULL;
	if ((vfprintf(stream, fmt, ap) < 0) ||
	    (fclose(stream) != 0)) {
		err = errno, free(buf), errno = err;		
		return NULL;
	}
	return buf;
}

/* TODO

char * dupfmts(const char *fmt, ...);

*/
