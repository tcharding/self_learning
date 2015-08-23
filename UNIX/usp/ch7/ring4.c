#include "tch.h"

static void ptrastr(const char *s, int fd, int n);
static void wastesometime(int n);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc;
	int fd[2];
	pid_t pid;
	int i, status, n;
	char buf[BUFSIZ];
	
	status = nproc = n = pid = 0;
	fd[0] = fd[1] = 0;
	if (argc != 2 && argc != 3)
		err_quit("Uasge: %s processes", argv[0]);
	if (argc == 3) {
		n = atoi(argv[2]);
		nproc = atoi(argv[1]);	
	} else 	{		/* argc == 2 */
		n = 0;
		nproc = atoi(argv[1]);	
	}
	if (nproc < 0 || n < 0)
		err_sys("atoi error");

	fprintf(stderr, "Creating ring of %d processes (n:%d)\n", nproc+1, n);
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

	if (snprintf(buf, BUFSIZ, "This is process: %d, ID: %ld parent: %ld\n",
		     i , (long)getpid(), (long)getppid()) < 0)
		err_sys("snprintf error");

	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	ptrastr(buf, STDERR_FILENO, n);
	return 0;
}

static void ptrastr(const char *s, int fd, int n)
{

	while (*s != '\0') {
		if (write(fd, s, 1) == -1)
			err_sys("write error");
		s++;
		wastesometime(n);
	}
}

/* wastesometime: */
static void wastesometime(int n)
{
	static volatile int dummy = 0;
	int i;

	for (i = 0; i < n; i++)
		dummy++;
}
