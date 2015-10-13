/* orginal file: ../names/daytimetcpcli.c */ 
#include "unp.h" 
 
int 
main(int argc, char **argv) 
{ 
	int sockfd, n; 
	char recvline[MAXLINE + 1]; 
	socklen_t len; 
	struct sockaddr_un servaddr;
 
	sockfd = Socket(AF_LOCAL, SOCK_STREAM, 0);
	
	bzero(&servaddr, sizeof(servaddr));
	servaddr.sun_family = AF_LOCAL;
	strcpy(servaddr.sun_path, UNIXSTR_PATH);

	Connect(sockfd, (SA *) &servaddr, sizeof(servaddr));
				/* SUN_LEN or sizeof */
	/* Connect(sockfd, (SA *) &servaddr, sizeof(servaddr)); */
	
	len = sizeof(servaddr); 
	Getpeername(sockfd, (SA *)&servaddr, &len); 
	printf("connected to %s\n", Sock_ntop_host((SA *)&servaddr, len)); 
	sleep(5);
	
	while ( (n = Read(sockfd, recvline, MAXLINE)) > 0) { 
		recvline[n] = 0; /* null terminate */ 
		fprintf(stderr, "nbytes read: %d\n", n);
		Fputs(recvline, stdout); 
	} 
	exit(0); 
} 
