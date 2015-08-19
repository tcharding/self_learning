#include "socket.h"
int main(int argc, char *argv[])
{
	int sd, n;
	char recvline[MAXLINE];
	socklen_t salen;
	struct sockaddr_storage *ssp;

	if (argc == 1)
		sd = Udp_client("localhost", "daytime", (SA **) &ssp, &salen);
	else if (argc == 2)
		err_quit("Usage: %s <hostname/IPaddress> <service/port#>");
	else 
		sd = Udp_client(argv[1], argv[2], (SA **) &ssp, &salen);
	
	printf("sending to %s\n", Sock_ntop_host((SA *) ssp, salen));

	Sendto(sd, "", 1, 0, (SA *) ssp, salen); /* send 1-byte datagram */

	n = Recvfrom(sd, recvline, MAXLINE, 0, NULL, NULL);
	recvline[n] = 0;	/* null terminate */
	Fputs(recvline, stdout);

	exit (0);
}
