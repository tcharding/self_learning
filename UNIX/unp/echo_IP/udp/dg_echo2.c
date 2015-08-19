#include "socket.h"

static void recvfrom_int(int);
static int count;

void dg_echo(int sockfd, struct sockaddr *pcliaddr, socklen_t clilen)
{
	socklen_t len;
	char msg[MAXLINE];

	Signal(SIGINT, recvfrom_int);

	for ( ; ; ) {
		len = clilen;
		Recvfrom(sockfd, msg, MAXLINE, 0, pcliaddr, &len);

		++count;
	}
}

static void recvfrom_int(int signo)
{
	printf("\nreceived %d datagrams\n", count);
	exit(0);
}
