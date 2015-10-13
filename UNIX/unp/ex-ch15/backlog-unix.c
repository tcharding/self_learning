#include "unp.h"

#define BACKLOG_MAX 14

static void sig_chld(int);
static void sig_alrm(int);
static volatile sig_atomic_t timeout;

/* determine maximum que for varios backlog values */
int main(void)
{
	int fd[2];		/* stream pipe */
	int sockfd;
	pid_t childpid;
	int i, backlog, n;
	struct sockaddr_un addr;
	char buf[MAXLINE];
	socklen_t len;
	
	timeout = 0;
	if (socketpair(AF_LOCAL, SOCK_STREAM, 0, fd) < 0)
		err_sys("socketpair error");

	Signal(SIGCHLD, sig_chld);
	Signal(SIGALRM, sig_alrm);
	
	if ( (childpid = Fork()) == 0) { 	/* child */
		Close(fd[0]);

		for ( ; ; ) {
			n = Read(fd[1], buf, sizeof(buf)); /* get from parent */
			buf[n] = '\0';			 
			backlog = atoi(buf);
			(void)close(sockfd);

			sockfd = Socket(AF_LOCAL, SOCK_STREAM, 0);

			unlink(UNIXSTR_PATH);
			
			bzero(&addr, sizeof(addr));
			addr.sun_family = AF_LOCAL;
			strcpy(addr.sun_path, UNIXSTR_PATH);

			Bind(sockfd, (SA *) &addr, SUN_LEN(&addr));
			Listen(sockfd, backlog);
			
			Write(fd[1], "1", 1); /* sync byte */
		}
		
	} else {		/* parent */
		Close(fd[1]);

		backlog = 0;
	inc_backlog:
		while (backlog < BACKLOG_MAX) {
			snprintf(buf, sizeof(buf), "%d", backlog);
			Write(fd[0], buf, strlen(buf));
			Read(fd[0], buf, 1); /* sync byte */
			for (i = 0 ; /* infinite */ ; i++ ) {
				(void)alarm(2);
				sockfd = Socket(AF_LOCAL, SOCK_STREAM, 0);
				
				bzero(&addr, sizeof(addr));
				addr.sun_family = AF_LOCAL;
				strcpy(addr.sun_path, UNIXSTR_PATH);

				len = sizeof(addr);
				if (connect(sockfd, (SA *) &addr, len) < 0) {
					if (errno == EINTR) {
						fprintf(stderr,
							"Backlog: %d max: %d\n",
							backlog, i);
						backlog++;
						goto inc_backlog;
					}
				}
				Close(sockfd); /* do we need this ? */
			}
		}
	}
}

static void sig_chld(int signo)
{
				/* exit is not async safe (apue) */
	_exit(0);		/* child is finished we are done */
}
static void sig_alrm(int signo)
{
	timeout = 1;		/* signal caught */	
}
