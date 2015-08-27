#include "tch.h"

/*
 * C String Library
 */ 

/* dups: duplicate s using memory streams, NULL on error with errno set */
char *s_dup(const char *s)
{
	FILE *fp;		
	size_t size;
	char *buf;
	int e;

	if (s == NULL)		/* normal condition */
		return NULL;

	buf = NULL;
	if ((fp = open_memstream(&buf, &size)) == NULL)
		return NULL;
	if (fputs(s, fp) < 0) {
		e = errno, free(buf), errno = e;
		return NULL;
	}
	if (fclose(fp) != 0) {
		e = errno, free(buf), errno = e;
		return NULL;
	}
	return buf;
}

/* TODO

char * dupfmts(const char *fmt, ...);

*/
