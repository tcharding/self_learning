#include "stdio.h"
#include <stdlib.h>
#include <unistd.h>

/* fclose: flush stream and close */
int fclose(FILE *fp)
{
	_flushbuf(EOF,fp);
	free(fp->base);
	return close(fp->fd);
}

int fflush(FILE *fp)
{
	return _flushbuf(EOF, fp);
}
