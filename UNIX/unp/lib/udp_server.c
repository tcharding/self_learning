#include "socket.h"

int udp_server(const char *host, const char *serv, socklen_t *addlenp)
{
	int sd, n;
	struct addrinfo hints, *res, *ressave;
	const int on = 1;
	bzero(&hints, sizeof(struct addrinfo));
	hints.ai_flags = AI_PASSIVE;
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;

	if ( (n = getaddrinfo(host, serv, &hints, &res)) != 0)
		err_sys("udp_server error for %s, %s: %s",
			host, serv, gai_strerror(n));

	ressave = res;

	do {
		sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
		if (sd < 0)
			continue; /* error - try next one */

		Setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
		if (bind(sd, res->ai_addr, res->ai_addrlen) == 0)
			break;	/* success */

		Close(sd);	/* bind error - cloase and try nex one */
	} while ( (res = res->ai_next) != NULL);

	if (res == NULL)	/* errno from final socket() or bind()f */
		err_sys("udp_server error for %s, %s", host, serv);

	freeaddrinfo(ressave);

	return (sd);
}

int Udp_server(const char *host, const char *serv, socklen_t *addlenp)
{
	return udp_server(host, serv, addlenp);
}
