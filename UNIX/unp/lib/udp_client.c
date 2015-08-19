#include "socket.h"

int udp_client(const char *host, const char *serv, SA **saptr, socklen_t *lenp)
{
	int sd, n;
	struct addrinfo hints, *res, *ressave;

	bzero(&hints, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;

	if ( (n = getaddrinfo(host, serv, &hints, &res)) != 0)
		err_quit("udp_client error for %s, %s: %s",
			 host, serv, gai_strerror(n));

	ressave = res;

	do {
		sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
		if (sd >= 0)
			break;	/* success */
	} while ( (res = res->ai_next) != NULL);

	if (res == NULL)	/* errno set from final socket */
		err_sys("udp_client error for %s, %s", host, serv);

	*saptr = Malloc(res->ai_addrlen);
	memcpy(*saptr, res->ai_addr, res->ai_addrlen);
	*lenp = res->ai_addrlen;

	freeaddrinfo(ressave);

	return (sd);
}

int Udp_client(const char *host, const char *serv, SA **saptr, socklen_t *lenp)
{
	return udp_client(host, serv, saptr, lenp);
}
