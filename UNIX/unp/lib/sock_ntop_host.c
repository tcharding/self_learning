#include "socket.h"
/* sock_ntop_host: get IP address as char ptr, static memory */ 
char *
sock_ntop_host(const struct sockaddr *sa, socklen_t salen)
{
    static char str[INET6_ADDRSTRLEN]; /* IPv6 is longest */

    switch (sa->sa_family) {
    case AF_INET: {
	struct sockaddr_in *sin = (struct sockaddr_in *) sa;

	if (inet_ntop(AF_INET, &sin->sin_addr, str, sizeof(str)) == NULL)
	    return(NULL);
	return(str);
    }


    case AF_INET6: {
	struct sockaddr_in6	*sin6 = (struct sockaddr_in6 *) sa;

	if (inet_ntop(AF_INET6, &sin6->sin6_addr, str, sizeof(str)) == NULL)
	    return(NULL);
	return(str);
    }


    default:
	snprintf(str, sizeof(str),
		 "sock_ntop_host: unknown AF_xxx: %d, len %d",
		 sa->sa_family, salen);
	return(str);
    }
    return (NULL);
}

/* wrapper */
char *
Sock_ntop_host(const struct sockaddr *sa, socklen_t salen)
{
    char *ptr;

    if ( (ptr = sock_ntop_host(sa, salen)) == NULL)
	err_sys("sock_ntop_host error");	/* inet_ntop() sets errno */
    return(ptr);
}
