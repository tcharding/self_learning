#define _DEFAULT_SOURCE
#include "socket.h"
#include <time.h>


/*
 * Allow second argument to explicitly set IP protocol
 */ 
int main (int argc, char *argv[])
{
	int listenfd, connfd;
	socklen_t len;
	char buf[MAXLINE];
	time_t ticks;
	struct sockaddr_storage cliaddr;
	char hbuf[NI_MAXHOST], sbuf[NI_MAXSERV];
	
	if (argc == 2)
		listenfd = Tcp_listen(NULL, argv[1], NULL);
	else if (argc == 3)
		listenfd = Tcp_listen(argv[1], argv[2], NULL);
	else
		err_quit("Usage: %s [ <host> ] <service or port#>", argv[0]);
	
	for ( ; ; ) {
		len = sizeof(cliaddr);
		connfd = Accept(listenfd, (SA *) &cliaddr, &len);
		/* printf("connection from %s\n", Sock_ntop((SA *) &cliaddr, len)); */
		if (getnameinfo((SA *) &cliaddr, len,
				hbuf, sizeof(hbuf),
				sbuf, sizeof(sbuf),
				NI_NUMERICHOST | NI_NUMERICSERV) == 0)
			printf("host=%s, serv=%s\n", hbuf, sbuf);
		else
			err_msg("getnameinfo error");
		
		ticks = time(NULL);
		snprintf(buf, sizeof(buf), "%.24s\r\n", ctime(&ticks));
		Write(connfd, buf, strlen(buf));

		Close(connfd);
	}
}
