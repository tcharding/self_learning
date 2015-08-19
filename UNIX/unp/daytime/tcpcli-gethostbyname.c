#include "socket.h"

int main(int argc, char *argv[])
{
	int sd, n;
	char recvline[MAXLINE];
	struct sockaddr_in servaddr;
	struct in_addr **pptr;
	struct in_addr *inetaddrp[2];
	struct in_addr inetaddr;
	struct hostent *hp;
	struct servent *sp;

	if (argc != 3)
		err_quit("usage: %s <hostname> <service>");

	if ( (hp =  gethostbyname(argv[1])) == NULL) {
		if (inet_pton(AF_INET, argv[1], &inetaddr) == 0) { /* fail on 0 */
			err_quit("hostname error for %s", argv[0]);
	
		} else {
			inetaddrp[0] = &inetaddr;
			inetaddrp[1] = NULL;
			pptr = inetaddrp;
		}
	} else {
		pptr = (struct in_addr **) hp->h_addr_list;
	}
	if ( (sp = getservbyname(argv[2], "tcp")) == NULL)
		err_quit("getservbyname error for %s", argv[2]);

	for ( ; *pptr != NULL; ++pptr) {
		sd = Socket(AF_INET, SOCK_STREAM, 0);
		bzero(&servaddr, sizeof(struct sockaddr_in));
		servaddr.sin_family = AF_INET;
		servaddr.sin_port = sp->s_port;
		memcpy(&servaddr.sin_addr, *pptr, sizeof(servaddr));
		printf("trying %s\n", Sock_ntop((SA *) &servaddr, sizeof(servaddr)));

		if (connect(sd, (SA *) &servaddr, sizeof(servaddr)) == 0)
			break;	/* success */
		err_ret("connect error");
		close(sd);
	}
	if(*pptr == NULL)
		err_quit("unable to connect");
	while ( (n= Read(sd, recvline, MAXLINE)) > 0) {
		recvline[n] = 0; /* null terminate */
		Fputs(recvline, stdout);
	}
	exit(0);
}
