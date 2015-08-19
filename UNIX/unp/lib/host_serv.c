#include "socket.h"

struct addrinfo *host_serv(const char *hostname,
			   const char *service,
			   int family,
			   int socktype)
{
	int n;
	struct addrinfo hints, *res;

	bzero(&hints, sizeof(struct addrinfo));
	hints.ai_flags = AI_CANONNAME; /* always return canonical name */
	hints.ai_family = family;	/* AF_UNSPEC, AF_INET, AF_INET6, ect. */
	hints.ai_socktype = socktype;	/* 0, SOCK_STREAM, SOCK_DGRAM */

	if ( (n = getaddrinfo(hostname, service, &hints, &res)) != 0)
		return NULL;

	return res;		/* return pointer to first on linked list */
}

/* wrapper */
struct addrinfo *Host_serv(const char *hostname,
			   const char *service,
			   int family,
			   int socktype)
{
	struct addrinfo *res;

	if ( (res = host_serv(hostname, service, family, socktype)) == NULL)
		err_sys("host_serv error");

	return res;
}
