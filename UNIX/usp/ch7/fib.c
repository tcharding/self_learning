#include "tch.h"
#include <stdarg.h>
#ifndef PIPE_BUF
#define PIPE_BUF 255
#endif

enum { IN, OUT };

static void child_fn(int nid);
static long fib_add(long a, long b);
static int read_seq(const char *buf, long *a, long *b);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	long a, b, c;
	int nproc, n;
	int fd[2];
	pid_t pid;
	int nid;		/* node number */
	int i, status;
	size_t len;		/* string length */
	ssize_t res;		/* for write(2) */
	char buf[PIPE_BUF];
				/* lint likes me if I do this */
	a = b = c = 0;
	status = nproc = n = 0;
	bzero(fd, sizeof(int)*2);
				/* check command line args */
	if (argc != 2 || ((n = atoi(argv[1])) < 0))
		err_quit("Uasge: fib n", argv[0]);
				/* define fib(0) and fib(1) */
	if (n < 3) {
		fprintf(stdout, "Fib(%d) = %ld\n", n, (long)1);
		exit(0);
	}
	nproc = n - 2;
	nid = 0;		/* original node */
				/* set up initial ring */
	Pipe(fd);
	Dup2(fd[IN], STDIN_FILENO);
	Dup2(fd[OUT], STDOUT_FILENO);
	Close(fd[IN]), Close(fd[OUT]);
				/* add nodes to ring */
	for (i = 1; i < nproc; i++) {
		Pipe(fd);
		if ((pid = Fork()) > 0)
			Dup2(fd[OUT], STDOUT_FILENO);
		else {
			Dup2(fd[IN], STDIN_FILENO);
			nid = i;
		}
		Close(fd[IN]), Close(fd[OUT]);
		if (pid > 0)	/* create process chain */
			break;
	}
	if (nid != 0) {
		child_fn(nid);	/* does not return */
	} 
				/* original parent node only continues here */
	if (snprintf(buf, PIPE_BUF, "%ld %ld", (long)1, (long)1) < 0)
		err_sys("snprnitf error");
	len = strlen(buf) + 1; /* +1 for '\0' */
	res = write(STDOUT_FILENO, buf, len);
	if ((res == -1) || ((size_t)res != len))
		err_sys("write error");

	if (read(STDIN_FILENO, buf, PIPE_BUF) == -1)
		err_sys("read error");
	if (read_seq(buf, &a, &b) == -1)
		err_quit("read_seq error");
	c = fib_add(a, b);
	fprintf(stderr, "Fib(%d) = %ld\n", n, c);
	return 0;
}

/* child_fn: do child labour for node nid */
static void child_fn(int nid)
{
	long a, b, c;
	char buf[PIPE_BUF];
	ssize_t res;
	size_t len;

	a = b = c = 0;
	bzero(buf, sizeof(buf));
	if (read(STDIN_FILENO, buf, PIPE_BUF) == -1)
		err_sys("read error");
	if ((read_seq(buf, &a, &b)) == -1)
		err_quit("[pid:%ld] read_seq error", (long)getpid());
	c = fib_add(a, b);
	if (snprintf(buf, PIPE_BUF, "%ld %ld", b, c) < 0)
		err_sys("snprintf error");
	len = strlen(buf) + 1;
	res = write(STDOUT_FILENO, buf, len);
	if ((res == -1) || ((size_t)res != len))
		err_sys("write error");
	/* fprintf(stderr, "Process %d with PID %ld and parent PID %ld", */
	/* 	nid, (long)getpid(), (long)getppid()); */
	/* fprintf(stderr, " received (%ld %ld) and sent (%ld %ld)\n", a, b, b, c); */
	exit(0);
	
}
/* fib_add: add a to b, checking for overflow */
static long fib_add(long a, long b)
{
	long c;

	c = a + b;
	if (c < a || c < b)
		err_quit("fib_add: overflow error");
	return c;
}
/* read_seq: read fib sequence into a and b from buf */
static int read_seq(const char *buf, long *a, long *b)
{
	int err;
	
	if (sscanf(buf, "%ld %ld", a, b) == EOF) {
		err = errno;
		err_msg("sscanf error");
		errno = err;
		return -1;
	}
	return 0;
}
