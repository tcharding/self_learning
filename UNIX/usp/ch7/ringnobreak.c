#include "tch.h"
#include <stdarg.h>

static void print_ring(pid_t a[], int size);
/* static void pprint_pid(pid_t child, char *fmt, ...); */
/* static void pprint_hdr(void); */

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc, nfork;
	int fd[2];
	pid_t pid;
	int i, k, status;
	pid_t *ida;
	pid_t next_ID;
	ssize_t res;
	
	status = nproc = nfork = pid = 0;
	fd[0] = fd[1] = 0;
	if (argc != 2 || ((nfork = atoi(argv[1])) <= 0))
		err_quit("Uasge: %s forks", argv[0]);

	/* exponential growth when neither process breaks fork loop */
	switch (nfork) {
	case 0:
		nproc = 1;
		break;
	case 1:
		nproc = 2;
		break;
	default:
		nproc = nfork * nfork;
		break;
	}
	fprintf(stderr, "Creating ring of %d processes\n", nproc);
	Pipe(fd);
	Dup2(fd[0], STDIN_FILENO);
	Dup2(fd[1], STDOUT_FILENO);
	Close(fd[0]);
	Close(fd[1]);
	
	for (i = 0; i < nfork; i++) {
		Pipe(fd);
		if ((pid = Fork()) > 0)
			Dup2(fd[1], STDOUT_FILENO);
		else
			Dup2(fd[0], STDIN_FILENO);
		Close(fd[0]);
		Close(fd[1]);
	}
	
	if ((ida = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	next_ID = getpid();
	ida[0] = next_ID;
	for (k = 0; k < nproc; k++) {
		res = write(STDOUT_FILENO, &next_ID, sizeof(next_ID));
		if ((res == -1) || ((size_t)res != sizeof(next_ID)))
			err_sys("write error");
		res = read(STDIN_FILENO, &next_ID, sizeof(next_ID));
		if ((res == -1) || (size_t)res != sizeof(next_ID))
			err_sys("read error");
		ida[k] = next_ID;
	}

	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	print_ring(ida, nproc);
	free(ida);
	return 0;
}

/* print_ring: print ring ID's to stderr */
static void print_ring(pid_t a[], int size)
{
	int i;

	fprintf(stderr, "[ ");
	for (i = 0; i < size; i++)
		fprintf(stderr, "%ld ", (long)a[i]);
	fprintf(stderr, "]\n");
}


/* /\* pprint_hdr: print process info header *\/ */
/* static void pprint_hdr(void) */
/* { */
/* 	fprintf(stderr, "( %-5s %-5s %-5s )\n", "ppid", "pid", "child"); */
/* } */
/* /\* printid: print process identifiers including child pid *\/ */
/* static void pprint_pid(pid_t child, char *fmt, ...) */
/* { */
/* 	FILE *fp; */
/* 	size_t size; */
/* 	char *buf; */
/* 	va_list ap; */

/* 	if ((fp = open_memstream(&buf, &size)) == NULL) */
/* 		err_sys("ms_open error"); */
/* 	fprintf(fp, "[ %-5ld %-5ld %-5ld ] ", */
/* 		(long)getppid(), (long)getpid(), (long)child); */

/* 	va_start(ap, fmt); */
/* 	vfprintf(fp, fmt, ap); */
/* 	va_end(ap); */
	
/* 	if (fclose(fp) != 0) */
/* 		err_sys("fclose error"); */
/* 	fprintf(stderr, "%s\n", buf); */
/* } */
