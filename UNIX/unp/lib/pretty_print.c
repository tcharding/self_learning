#include "unp.h"

/* pretty print socket address information */
void pp_sock(const struct sockaddr *sa, socklen_t salen)
{
	char portstr[8]; 
	static char str[128]; /* Unix domain is largest */ 

	switch (sa->sa_family) {
	case AF_INET: {
		struct sockaddr_in *sin = (struct sockaddr_in *) sa; 
		if (inet_ntop(AF_INET, &sin->sin_addr, str, (socklen_t)sizeof(str))
		    == NULL)
			err_quit("inet_ntop error");
		if (ntohs(sin->sin_port) != 0) { 
			(void)snprintf(portstr, sizeof(portstr), ":%d",
				 (int)ntohs(sin->sin_port)); 
			(void)strcat(str, portstr); 
		}
		fprintf(stderr, "Socket: AF_INET, %s\n", str);
		break;
	}
	default:
		err_sys("dump_sock: family type not supported");
	}
}
