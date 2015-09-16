#include "apue.h"

enum {IN, OUT};			/* pipe fd's */

static void	sig_pipe(int);		/* our signal handler */

/* explore behaviour of select when write end of pipe is closed */
int main(void)
{
	int fd[2];
	pid_t pid;
	fd_set rset;
	int maxfd;
	int nready, nbytes;
	char buf[BUFSIZ];
	
	Pipe(fd);

	if (signal(SIGPIPE, sig_pipe) == SIG_ERR)
		err_sys("signal error");

	if ((pid = Fork()) == 0) { /* child */
		Close(fd[IN]);
		sleep(2);	/* let parent run first */
		/* write(fd[OUT], "pipe write!", 11); */
		Close(fd[OUT]);	/* close the write end */
	} else {		/* parent */
		Close(fd[OUT]);
		maxfd = fd[IN];
		for ( ; ; ) {
			FD_ZERO(&rset);
			FD_SET(fd[IN], &rset);
			FD_SET(STDIN_FILENO, &rset);
			if ((nready = select(maxfd+1, &rset, NULL, NULL, NULL)) < 0)
				err_sys("select error");
			fprintf(stderr, "Select returned: %d\n", nready);
			if (FD_ISSET(fd[IN], &rset)) {
				if ((nbytes = read(fd[IN], buf, BUFSIZ)) == 0) {
					err_msg("read got 0 bytes");
					break;
				}
				fprintf(stderr, "read from pipe nbytes: %d\n", nbytes);
				buf[nbytes] = '\0';
				if (nbytes == EOF)
					err_msg("pipe read got EOF");
				fprintf(stderr, "read from pipe: %s\n", buf);
			}
			if (FD_ISSET(STDIN_FILENO, &rset)) {
				if ((nbytes = read(STDIN_FILENO, buf, BUFSIZ)) < 0)
					err_sys("read error");
				buf[nbytes] = '\0';
				fprintf(stderr, "read from stdin: %s\n", buf);
			}
		}
		
	}
	sleep(2);
	exit(0);
}

static void sig_pipe(int signo)
{
	printf("SIGPIPE caught\n");
	exit(1);
}
