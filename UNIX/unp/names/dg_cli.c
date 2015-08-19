#define _DEFAULT_SOURCE
#include "socket.h"

void dg_cli(FILE *fp, int sockfd, const SA *pservaddr, socklen_t servlen)
{
	int n;
	char **pptr;
	char sendline[MAXLINE], recvline[MAXLINE + 1];
	socklen_t len;
	struct sockaddr_in *preply_addr;
	struct hostent *nhptr, *ahptr;
	char str[INET_ADDRSTRLEN];
	
	preply_addr = Malloc(servlen);
	
	while (Fgets(sendline, MAXLINE, fp) != NULL) {
		Sendto(sockfd, sendline, strlen(sendline), 0, pservaddr, servlen);

		len = servlen;
		n = Recvfrom(sockfd, recvline, MAXLINE, 0, (SA *) preply_addr, &len);

		recvline[n] = 0;		/* null terminator */
		Fputs(recvline, stdout);

		if ( (ahptr = gethostbyaddr(&preply_addr->sin_addr,
					    sizeof(struct in_addr),
					    AF_INET)) == NULL) {
			Inet_ntop(AF_INET, (char *)preply_addr,
				  str, sizeof(struct sockaddr_in));
			err_msg("gethostbyaddr error for %s", str);
			continue;
		}
		
		if ( (nhptr = gethostbyname(ahptr->h_name)) == NULL) 
			err_msg("gethostbyname error for %s", ahptr->h_name);

		switch (nhptr->h_addrtype) {
		case AF_INET:
			pptr = nhptr->h_addr_list;
			for ( ; *pptr != NULL; pptr++) 
				printf("\taddress: %s\n",
				       Inet_ntop(nhptr->h_addrtype,
						 *pptr, str, sizeof(str)));
			break;
		default:
			err_ret("unknown address type");
			break;
		}
	}
}

