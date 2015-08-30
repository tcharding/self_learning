#include "tch.h"
#include "data.h"
#include "string.h"
#include <stdarg.h>

/*
 * Abstract Data Type (transformation (ringpp.c))
 */

/* adt_alloc: allocate memory for new adt */
data_t *adt_alloc(void)
{
	data_t *d;
	
	if ((d = (data_t *)malloc(sizeof(data_t))) == NULL)
		return NULL;
	bzero(d, sizeof(data_t));
	return d;
}

/* adt_creat: allocate memory for new adt and initialise */
data_t *adt_creat(char c, const char *s)
{
	data_t *d;
	int err;

	if ((d = adt_alloc()) == NULL)
		return NULL;
	d->c = c;
	errno = 0;		/* clear errno */
	d->s = s_dup(s);	/* may be NULL */
	if (errno != 0) {
		err = errno, adt_free(d), errno = err;
		return NULL;
	}
	return d;
}

/* adt_dup: return duplicate of d, must be free'd with adt_free */
data_t *adt_dup(data_t *d)
{
	if (d == NULL) {
		errno = EINVAL;
		return NULL;
	}
	return (adt_creat(d->c, d->s));
}
void adt_free(data_t *d)
{
	if (d != NULL) {
		if (d->s != NULL)
			free(d->s);
		free(d);
	}
}

/* adt_tostring: return string formed from d, must be free'd */
char *adt_tostring(data_t *d)
{
	FILE *fp;		
	size_t size;
	char *buf;
	int e;
	char *s;

	if (d == NULL) {
		errno = EINVAL;
		return NULL;
	}
	buf = NULL;
	if ((fp = open_memstream(&buf, &size)) == NULL)
		return NULL;

	s = (d->s == NULL) ? "<NULL>" : d->s;
	if (fprintf(fp, "[%c -> %s]", d->c, s) < 0) {
		e = errno, free(buf), errno = e;
		return NULL;
	}
	if (fclose(fp) != 0) {
		e = errno, free(buf), errno = e;
		return NULL;
	}
	return buf;

}
/* adt_err_doit: error helper */
static void adt_err_doit(data_t *d,int errnoflag, int error,
			 const char *fmt, va_list ap)
{
	FILE *fp;
	char *buf;
	size_t size;

	buf = NULL;
	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");

	if (d == NULL)
		(void)fputs("<NULL data_t>", fp);
	else {
		char *s;
		s = (d->s == NULL) ? "<NULL>" : d->s;
		(void)fprintf(fp, "[%c -> %s]: ", d->c, s);
	}
	(void)vfprintf(fp, fmt, ap);
	if (errnoflag == 1)
		fprintf(fp, ": %s", strerror(error));
	(void)fprintf(fp, "\n");
	(void)fclose(fp);
	(void)fflush(stdout);		/* in case stdout and stderr are the same */
	(void)fputs(buf, stderr);
	(void)fflush(NULL);		/* flushes all stdio output streams */
	free(buf);		/* free memstream resource */
	
}

void adt_err_sys(data_t *d, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	adt_err_doit(d, 1, errno, fmt, ap);
	va_end(ap);
	exit(EXIT_FAILURE);
}

void adt_err_msg(data_t *d, char *fmt, ...)
{
	va_list ap;
		
	va_start(ap, fmt);
	adt_err_doit(d, 0, 0, fmt, ap);
	va_end(ap);
	return;
}

/* adt_pprint: print function */
int adt_pprint(data_t *d)
{
	return printf("%c: s:%s\n", d->c, d->s);
}
