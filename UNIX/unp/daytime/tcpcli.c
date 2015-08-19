#include "socket.h"

/* defined in getnameinfo_timeo.c */
int getnameinfo_timeo(const struct sockaddr *sa, socklen_t salen,
		      char *host, socklen_t hostlen,
		      char *serv, socklen_t servlen, int flags, int sec);

int main(int argc, char *argv[])
{
	int sd, n, res;
	char recvline[MAXLINE + 1];
	socklen_t len;
	struct sockaddr_storage ss;
	char host[INET6_ADDRSTRLEN], serv[INET6_ADDRSTRLEN];

	if (argc != 3)
		err_quit("Usage: %s <hostname/IPaddress> <service/port#>", argv[0]);

	sd = Tcp_connect(argv[1], argv[2]);

	len = sizeof(ss);
	Getpeername(sd, (SA *) &ss, &len);

	if ( (res = getnameinfo_timeo((SA *) &ss, len, host,
				     sizeof(host), serv, sizeof(serv),
				     NI_NUMERICHOST, 1)) != 0)
		err_msg("getnameinfo error: %d", res);
	else 
		printf("Connected to %s\n", host);
	/*printf("Connected to %s\n", Sock_ntop_host((SA *) &ss, len));*/

	while ( (n = Read(sd, recvline, MAXLINE)) > 0) {
		recvline[n] = 0; /* null terminate */
		Fputs(recvline, stdout);
	}
	exit(0);
}
