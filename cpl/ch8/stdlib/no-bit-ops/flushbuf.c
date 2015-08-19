#include "stdio.h"
#include <stdlib.h>
#include <unistd.h>

/* _flushbuf: write stream buffer to stdout */
int _flushbuf(int c, FILE *fp)
{
	int bufsize, nbytes;

	if ((fp->flag&(_WRITE|_EOF|_ERR)) != _WRITE)
		return EOF;
	bufsize = (fp->flag & _UNBUF) ? 1 : BUFSIZ;
	if (fp->base == NULL) {	/* no buffer yet */
		if ((fp->base = (char *) malloc(bufsize)) == NULL)
			return EOF; /* cannot get buffer */
		fp->cnt = bufsize;
		fp->ptr = fp->base;
	}
	nbytes = fp->ptr - fp->base;
	if (nbytes > 0) {
		if (write(fp->fd, fp->base, nbytes) < nbytes)
			return EOF;
		fp->cnt = bufsize;
		fp->ptr = fp->base;
	}
	if (c >= 0) {
		*fp->ptr++ = c;
		fp->cnt--;
	}
	return 0;
}
