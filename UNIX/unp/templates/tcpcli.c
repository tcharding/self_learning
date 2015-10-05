#include "unp.h" 
#include "utils.h"

#define PORT 
 
int 
main(int argc, char **argv) 
{ 
	int sockfd;
	ssize_t n;
	char recvline[MAXLINE + 1]; 
	struct sockaddr_in servaddr; 
	char *s;		/* server address string */

	s = NULL, recvline[0] = '\0'; /* quiet lint */
	switch (argc) {
	case '1':
		s = LOCAL_HOST;
		break;
	case '2':
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
 
	while ( (n = read(sockfd, recvline, MAXLINE)) > 0) { 
		recvline[n] = '\0'; /* null terminate */ 
		if (fputs(recvline, stdout) == EOF) 
			err_sys("fputs error"); 
	} 
	if (n < 0) 
		err_sys("read error"); 
 
	exit(0); 
} 
