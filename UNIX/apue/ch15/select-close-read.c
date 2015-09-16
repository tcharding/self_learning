#include "apue.h"

enum {IN, OUT};			/* pipe fd's */

static void	sig_pipe(int);		/* our signal handler */
static void doexit(void);

/* explore behaviour of select when write end of pipe is closed */
int main(void)
{
	int fd[2];
	pid_t pid;
	fd_set wset;
	int maxfd;
	int nready;
	
	Pipe(fd);
	TELL_WAIT();

	if (signal(SIGPIPE, sig_pipe) == SIG_ERR)
		err_sys("signal error");
	if (atexit(doexit) != 0)
		err_sys("atexit error");
	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid == 0) {	/* child */
		Close(fd[IN]);	/* not used by child */
		fprintf(stderr, "child waiting\n");
		WAIT_PARENT();
		fprintf(stderr, "child starting\n");

		FD_ZERO(&wset);
		FD_SET(fd[OUT], &wset);
		maxfd = fd[OUT];
		fprintf(stderr, "calling select\n");
		if ((nready = select(maxfd+1, NULL, &wset, NULL, NULL)) < 0)
			err_sys("select error");
		fprintf(stderr, "select returned: %d\n", nready);
		if (FD_ISSET(fd[OUT], &wset)) {
			if (write(fd[OUT], "pipe write!", 11) < 0)
				err_sys("write to pipe failed");
		}
		Close(fd[OUT]);	/* close the write end */
	} else {		/* parent */
		Close(fd[OUT]);	/* not used by parent */
		sleep(2);
		fprintf(stderr, "parent closing read end of pipe\n");
		Close(fd[IN]);	/* close read end of pipe */
		sleep(2);
		fprintf(stderr, "parent TELL_CHILD\n");
		TELL_CHILD(pid);
		sleep(2);
		fprintf(stderr, "parent exiting\n");
		exit(0);
	}
	fprintf(stderr, "someone is exiting\n");
	exit(0);
}

static void sig_pipe(int signo)
{
	printf("SIGPIPE caught\n");
	exit(1);
}
static void doexit(void)
{
	fprintf(stderr, "running exit routine\n");
}
