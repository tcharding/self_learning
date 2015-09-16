#include "apue.h"
#include <limits.h>
#include <poll.h>

#define INFTIM -1
#define NFDS 2			/* num file descriptors to poll */

enum {IN, OUT};			/* pipe fd's */

static void	sig_pipe(int);		/* our signal handler */

/* explore behaviour of poll when write end of pipe is closed */
int main(void)
{
	int fd[2];
	pid_t pid;
	int nready, nbytes, maxi, i;
	struct pollfd pfds[NFDS];
	char buf[BUFSIZ];
	
	Pipe(fd);

	if (signal(SIGPIPE, sig_pipe) == SIG_ERR)
		err_sys("signal error");

	if ((pid = Fork()) == 0) { /* child */
		Close(fd[IN]);
		sleep(2);	/* let parent run first */
		write(fd[OUT], "pipe write!", 11);
		Close(fd[OUT]);	/* close the write end */
	} else {		/* parent */
		Close(fd[OUT]);
		pfds[0].fd = fd[IN];
		pfds[0].events = POLLRDNORM;
		pfds[1].fd = STDIN_FILENO;
		pfds[1].events = POLLRDNORM;
		maxi = NFDS-1;	/* max index into array */
		for ( ; ; ) {
			if ((nready = poll(pfds, maxi + 1, INFTIM)) < 0)
				err_sys("poll error");
			for (i = 0; i < NFDS; i++) {
				if (pfds[0].revents & POLLRDNORM) {
					nbytes = Read(fd[IN], buf, BUFSIZ);
					if (nbytes == 0) {
						err_msg("read got 0 bytes");
						exit(1);
					}
				}
				buf[nbytes] = '\0';
				fprintf(stderr, "read %d bytes from fd [%d]: %s\n",
					i, nbytes, buf);
			}
		}
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
