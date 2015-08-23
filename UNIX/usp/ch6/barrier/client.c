#include "tch.h"
#include "barrier.h"
#include <sys/wait.h>
#include <stdarg.h>		/* ISO C variable arguments */

void printid(pid_t child, char *fmt, ...);
void printid_header(void);

/* Synchronize process fan using barrier server */
int main(int argc, char *argv[])
{
	pid_t child = 0;
	int i, n;
	char *name;
	
	if (argc != 3) {
		fprintf(stderr, "Usage: %s barrier processes\n", argv[0]);
		return 1;
	}
	name = argv[1];
	n = atoi(argv[2]);  
	printid_header();
				/* create process fan */
	for (i = 1; i < n; i++) 
		if ((child = fork()) <= 0)
			break;
	if (child == -1)
		err_sys("fork error");

	printid(child, "before barrier");
	if (waitbarrier(name) == -1)
		err_sys("[pid:%ld] waitbarrier error\n", (long)getpid());
	printid(child, "through barrier");

	
	/* if (waitbarrier(name) == -1) */
	/* 	err_sys("[pid:%ld] waitbarrier error\n", (long)getpid()); */

	/* fprintf(stderr, "[pid:%ld] Through barrier 2\n", (long)getpid()); */
	return 0;
}

void printid_header(void)
{
	fprintf(stderr, "( %-5s %-5s %-5s )\n", "ppid", "pid", "child");
}
/* printid: print process identifiers including child pid */
void printid(pid_t child, char *fmt, ...)
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
