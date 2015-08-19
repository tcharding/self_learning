#include "apue.h"
#include <stdarg.h>

FILE *open_memstream(char **ptr, size_t *sizeloc); /* shouldn't need this */

int m_open_memstream(struct mems *m)
{
	if ((m->stream = open_memstream(&m->buf, &m->size)) == NULL)
		return -1;
	return 0;
}

/* m_insert: write fmt string to memory stream, -1 on error */
int m_insert(struct mems *m, char *fmt, ...)
{
	va_list ap;
	int retval;
	
	va_start(ap, fmt);
	retval = vfprintf(m->stream, fmt, ap);
	fflush(m->stream);
	va_end(ap);
	return retval;
}
