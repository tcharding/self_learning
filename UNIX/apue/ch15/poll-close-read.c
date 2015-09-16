#include "apue.h"
#include <limits.h>
#include <poll.h>

#define INFTIM -1

enum {IN, OUT};			/* pipe fd's */

static void	sig_pipe(int);		/* our signal handler */

/* explore behaviour of poll when write end of pipe is closed */
int main(void)
{
	int fd[2];
	pid_t pid;
	int nready, nbytes, maxi, i;
	struct pollfd pfds[1];
	char buf[BUFSIZ];
	
	Pipe(fd);

	if (signal(SIGPIPE, sig_pipe) == SIG_ERR)
		err_sys("signal error");

	if ((pid = Fork()) == 0) { /* child */
		Close(fd[IN]);
		sleep(2);	/* let parent run first */
				/* poll for pipe ready to write */
		pfds[0].fd = fd[IN];
		pfds[0].events = POLLOUT;
		maxi = 0;	/* max index into array */
		for ( ; ; ) {
			if ((nready = poll(pfds, maxi + 1, INFTIM)) < 0)
				err_sys("poll error");
			fprintf(stderr, "poll returned: %d\n", nready);
			if (pfds[0].revents & POLLOUT) {
				nbytes = write(fd[OUT], "pipe write!", 11);
				if (nbytes == -1)
					err_sys("write error");
				else
					err_msg("we wrote %d bytes\n", nbytes);
			} else {
				fprintf(stderr, "poll notout\n");
				exit(0);
			}
		}
		(void)close(fd[OUT]);	/* close the write end */
		fprintf(stderr, "child exiting\n");
		exit(0);
	} else {		/* parent */
		Close(fd[OUT]);
		fprintf(stderr, "parrent closing read end of pipe\n");
		Close(fd[IN]);
		fprintf(stderr, "parrent exiting\n");
		exit(0);
	}
	sleep(2);
	exit(0);
}

/* sig catcer */
static void sig_pipe(int signo)
{
	printf("SIGPIPE caught\n");
	exit(1);
}
