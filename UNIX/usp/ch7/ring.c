#include "tch.h"

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc;
	int fd[2];
	pid_t pid;
	int i, status;
	
	nproc = pid = status = 0;
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

	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	
	fprintf(stderr, "This is process: %d, ID: %ld parent: %ld\n",
	       i , (long)getpid(), (long)getppid());

	return 0;
}
