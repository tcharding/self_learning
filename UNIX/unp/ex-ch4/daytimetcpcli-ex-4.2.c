#include "unp.h" 

#define PORT 13		/* daytime server */

static void dump_sock(const struct sockaddr *sa, socklen_t salen);
 
int 
main(int argc, char **argv) 
{ 
	int sockfd;
	ssize_t n;
	char recvline[MAXLINE + 1]; 
	struct sockaddr_in servaddr, myaddr;
	char *s;		/* server address string */
	socklen_t len;
	
	s = NULL, recvline[0] = '\0'; /* quiet lint */
	switch (argc) {
	case 1:
		s = "127.0.0.1";
		break;
	case 2:
		s = argv[1];
		break;
	default:
		err_quit("usage: %s [<IPaddress>]", argv[0]);
		break;
	}

	if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) 
		err_sys("socket error");

	bzero(&servaddr, sizeof(servaddr)); 
	servaddr.sin_family = AF_INET; 
	servaddr.sin_port = htons(PORT);
	if (inet_pton(AF_INET, s, &servaddr.sin_addr) <= 0) 
		err_quit("inet_pton error for %s", argv[1]); 
 
	if (connect(sockfd, (SA *) &servaddr, (socklen_t)sizeof(servaddr)) < 0) 
		err_sys("connect error");
	len = (socklen_t) sizeof(struct sockaddr_in);
	if (getsockname(sockfd, (SA *) &myaddr, &len) < 0)
		err_sys("getsockname error");

	dump_sock((SA *) &myaddr, (socklen_t)sizeof(myaddr));
	
	while ( (n = read(sockfd, recvline, MAXLINE)) > 0) { 
		recvline[n] = '\0'; /* null terminate */ 
		if (fputs(recvline, stdout) == EOF) 
			err_sys("fputs error"); 
	} 
	if (n < 0) 
		err_sys("read error"); 
 
	exit(0); 
} 

/* pretty print socket address information */
static void dump_sock(const struct sockaddr *sa, socklen_t salen)
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

