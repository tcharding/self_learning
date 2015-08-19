#include "socket.h"
#include <time.h>
/*
 * Allow second argument to explicitly set IP protocol
 */ 
int main (int argc, char *argv[])
{
	int sd;
	socklen_t len;
	char buf[MAXLINE];
	time_t ticks;
	struct sockaddr_storage cliaddr;

	if (argc == 1) {
		fprintf(stderr, "starting udp daytime server at localhost:13\n");
		sd = Udp_server("0::0",  "daytime", NULL);
	} else if (argc == 2) {
		if ( (strcmp(argv[1], "--help") == 0) ||
		     (strcmp(argv[1], "-h") == 0))
			err_quit("Usage: %s [ <host> ] <service or port#>",
				 argv[0]);
		else
			sd= Udp_server(NULL, argv[2], NULL);
	} else if (argc == 3) {
		 sd = Udp_server(argv[1], argv[2], NULL);
	}		
       
	for ( ; ; ) {
		len = sizeof(cliaddr);
		Recvfrom(sd, buf, MAXLINE, 0, (SA *) &cliaddr, &len);
		printf("datagram from %s\n", Sock_ntop((SA *) &cliaddr, len));
		
		ticks = time(NULL);
		snprintf(buf, sizeof(buf), "%.24s\r\n", ctime(&ticks));
		Sendto(sd, buf, strlen(buf), 0, (SA *) &cliaddr, len);

		/* Close(sd); */
	}
}
