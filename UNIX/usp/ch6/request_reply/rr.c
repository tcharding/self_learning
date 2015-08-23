#include "tch.h"
#include "rr.h"
#include <errno.h>
#include <stdarg.h>

/* rr_pipe_name: fill fifo with name of pipe, memory allocated with malloc() */
int rr_pipe_name(pid_t pid, char **fifo)
{
	FILE *fp;
	char *buf;
	size_t size;
	int e;

	if (fifo == NULL || pid <= 0) {
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
	*fifo = buf;
	return 0;
}

/* pprint_hdr: print process info header */
void pprint_hdr(void)
{
	fprintf(stderr, "( %-5s %-5s %-5s )\n", "ppid", "pid", "child");
}
/* printid: print process identifiers including child pid */
void pprint_pid(pid_t child, char *fmt, ...)
{
	FILE *fp;
	size_t size;
	char *buf;
	va_list ap;

	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("ms_open error");
	fprintf(fp, "[ %-5ld %-5ld %-5ld ] ",
		(long)getppid(), (long)getpid(), (long)child);

	va_start(ap, fmt);
	vfprintf(fp, fmt, ap);
	va_end(ap);
	
	if (fclose(fp) != 0)
		err_sys("fclose error");
	fprintf(stderr, "%s\n", buf);
}
