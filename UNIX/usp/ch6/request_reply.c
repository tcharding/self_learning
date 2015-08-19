#include "tch.h"
#include "request_reply.h"
#include <errno.h>

/* rr_pipename: fill buf with name of pipe, memory allocated with malloc() */
int rr_pipename(pid_t pid, char **fifos)
{
	FILE *fp;
	char *buf;
	size_t size;
	int e;

	if (fifos == NULL || pid <= 0) {
		errno = EINVAL;
		return -1;
	}
	if ((fp = open_memstream(&buf, &size)) == NULL)
		return -1;
	if ((fprintf(fp, "%ld", (long)pid) < 0) ||
	    (fprintf(fp, ".release") < 0)) {
		e = errno, free(buf), errno = e;
		return -1;
	}
	if (fclose(fp) != 0) {
		e = errno, free(buf), errno = e;
		return -1;
	}
	*fifos = buf;
	return 0;
}
