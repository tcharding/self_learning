#include "socket.h"

void str_echo(int sockfd);
void sig_chld(int signo);
	
int
main(int argc, char *argv[])
{
	int listenfd, connfd;
	pid_t childpid;
	socklen_t clilen;
	struct sockaddr_in cliaddr, servaddr;
	
	/* create socket */
	listenfd = Socket(AF_INET, SOCK_STREAM, 0);

	/* set up server socket address */
	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port = htons(SERV_PORT);

	Bind(listenfd, (SA *) &servaddr, sizeof(servaddr));

	Listen(listenfd, LISTENQ);
	Signal(SIGCHLD, sig_chld);	/* must call waitpid */
	
	for ( ; ; ) {
		clilen = sizeof(cliaddr);
		if ( (connfd = Accept(listenfd, (SA *) &cliaddr, &clilen)) < 0) {
			if (errno == EINTR)
				continue;		/* back to for loop */
			else
				err_sys("accept error");
		}
		if ( (childpid = Fork()) == 0) { /* child process */
			Close(listenfd);			 /* close listening socket */
			str_echo(connfd);			 /* process request */
			Close(connfd);
			exit(EXIT_SUCCESS);
		}
		Close(connfd);	/* parent closes connected socket */
	}
}
