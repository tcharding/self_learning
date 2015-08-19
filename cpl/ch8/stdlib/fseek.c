#include "stdio.h"

enum {
	SEEK_SET = 0,
	SEEK_CURRENT = 1,
	SEEK_END = 2,
};

/* fseek: set file position indicator for stream */
int fseek(FILE *stream, long offset, int whence)
{
	int res;
	
	if (whence == SEEK_SET || whence == SEEK_CURRENT || whence == SEEK_END) 
		res = lseek(fp->fd, offset, whence);
	else 
		res = lseek(fp->fd, whence + offset, SEEK_SET);

	return (res == -1) ? -1 : 0;
}
