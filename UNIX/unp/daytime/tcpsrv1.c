#include "socket.h"
#include <time.h>

int main (int argc, char *argv[])
{
	int listenfd, connfd;
	socklen_t len;
	char buf[MAXLINE];
	time_t ticks;
	struct sockaddr_storage cliaddr;
	
	if (argc != 2)
		err_quit("Usage: %s <service or port#>", argv[0]);
	
	listenfd = Tcp_listen(NULL, argv[1], NULL);

	for ( ; ; ) {
		len = sizeof(cliaddr);
		connfd = Accept(listenfd, (SA *) NULL, &len);
		printf("connection from %s\n", Sock_ntop((SA *) &cliaddr, len));

		
		ticks = time(NULL);
		snprintf(buf, sizeof(buf), "%.24s\r\n", ctime(&ticks));
		Write(connfd, buf, strlen(buf));

		Close(connfd);
	}
}
