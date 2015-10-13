#include "unp.h"

#define BACKLOG_MAX 14
#define ADDR "127.0.0.1"

int pipefd[2];
#define pfd pipefd[0]
#define cfd pipefd[1]

static void sig_alrm(int);

/* determine maximum que for varios backlog values */
int main(void)
{
	int sockfd, listenfd, connfd;
	struct sockaddr_in addr, cliaddr;
	socklen_t clilen, addrlen;
	pid_t childpid;
	int i, backlog, junk;
	/* char buf[MAXLINE]; */
	int on = 1;
	int maxfd;
	fd_set rset;
	
	if (socketpair(AF_LOCAL, SOCK_STREAM, 0, pipefd) < 0)
		err_sys("socketpair error");

	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(SERV_PORT);
	
	if ( (childpid = Fork()) == 0) { 	/* child */
		Close(pfd);
		Read(cfd, &backlog, sizeof(int)); /* wait parent */
			
		listenfd = Socket(AF_INET, SOCK_STREAM, 0);
		Setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR,
			   &on, sizeof(on));
		addr.sin_addr.s_addr = htonl(INADDR_ANY);
		Bind(listenfd, (SA *) &addr, sizeof(addr));
		Listen(listenfd, backlog);

		maxfd = max(cfd, listenfd);
			
		Write(cfd, &junk, sizeof(int)); /* tell parent */

		for ( ; ; ) {
			FD_ZERO(&rset);
			FD_SET(listenfd, &rset);
			FD_SET(cfd, &rset);
	
			/* nsel = Select(maxfd + 1, &rset, NULL, NULL, NULL); */
			(void)Select(maxfd + 1, &rset, NULL, NULL, NULL);
			if (FD_ISSET(listenfd, &rset)) {
				clilen = sizeof(cliaddr);
				if ( (connfd = accept(listenfd, (SA *) &cliaddr, &clilen)) < 0) {
					if (errno == EINTR)
						continue;		/* back to for() */
					else
						err_sys("accept error");
				}

				if ( (childpid = Fork()) == 0) {	/* child process */
					Close(listenfd);	/* close listening socket */
					pause();		/* just keep the connection open */
					exit(0);
				}
				Close(connfd);			/* parent closes connected socket */				
			}
			if (FD_ISSET(cfd, &rset)) {
				exit(0);
				/* kill all childeren */
				Close(listenfd);
			}
		} /* back to start of for loop */
		
	} else {		/* parent */
		Close(cfd);
		Signal(SIGALRM, sig_alrm);

		for (backlog = 0; backlog < 1; backlog++) {
		/* for (backlog = 0; backlog < BACKLOG_MAX; backlog++) { */
			Write(pfd, &backlog, sizeof(int)); /* tell child */
			Read(pfd, &junk, sizeof(int)); /* wait child */
			for (i = 0 ; /* infinite */ ; i++ ) {
				(void)alarm(2);
				sockfd = Socket(AF_INET, SOCK_STREAM, 0);
				
				bzero(&addr, sizeof(addr));
				addr.sin_family = AF_INET;
				addr.sin_port = htons(SERV_PORT);
				if (inet_pton(AF_INET, "127.0.0.1",
					      &addr.sin_addr) <= 0)
					err_quit("inet_pton error");

				addrlen = sizeof(addr);
				if (connect(sockfd, (SA *) &addr, addrlen) < 0) {
					if (errno == EINTR) {
						fprintf(stderr,
							"Backlog: %d max: %d\n",
							backlog, i);
						backlog++;
						err_quit("parent exiting\n");
					}
				}
			}
		}
	}
}

static void sig_alrm(int signo)
{
				/* stdio in sig catcher, naughty naughty */
	fprintf(stderr, "caught sig_alrm\n"); 
	return;
}
