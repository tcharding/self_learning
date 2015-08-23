#include "tch.h"

void print_ring(pid_t a[], int size);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc;
	int fd[2];
	pid_t pid;
	int i, k, status;
	pid_t *ida;
	pid_t next_ID;
	
	status = nproc = pid = 0;
	fd[0] = fd[1] = 0;
	if (argc != 2 || ((nproc = atoi(argv[1])) <= 0))
		err_quit("Uasge: %s processes", argv[0]);

	fprintf(stderr, "Creating ring of %d processes\n", nproc+1);
	Pipe(fd);
	Dup2(fd[0], STDIN_FILENO);
	Dup2(fd[1], STDOUT_FILENO);
	Close(fd[0]);
	Close(fd[1]);
	
	for (i = 0; i < nproc; i++) {
		Pipe(fd);
		if ((pid = Fork()) > 0)
			Dup2(fd[1], STDOUT_FILENO);
		else
			Dup2(fd[0], STDIN_FILENO);
		Close(fd[0]);
		Close(fd[1]);
		if (pid > 0)
			break;
	}
	if ((ida = calloc(nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	next_ID = getpid();
	ida[0] = next_ID;

	for (k = 1; k < nproc; k++) {
		fprintf(stderr,"%ld\n", (long)next_ID);
		fprintf(stdout,"%ld\n", (long)next_ID);
		if (scanf("%ld", &next_ID) == EOF)
			err_sys("scanf error");
		/* if (read(STDIN_FILENO, &next_ID, sizeof(pid_t)) == -1) */
		/* 	err_sys("read error"); */
		ida[k] = next_ID;
	}
	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	print_ring(ida, nproc);
	return 0;
}

/* print_ring: print ring ID's to stderr */
void print_ring(pid_t a[], int size)
{
	int i;

	fprintf(stderr, "[%ld] ", (long)getpid());
	for (i = 0; i < size; i++)
		fprintf(stderr, "%ld", (long)a[i]);
	fprintf(stderr, "\n");
}
