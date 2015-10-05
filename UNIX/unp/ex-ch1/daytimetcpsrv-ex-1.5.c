/* orginal file: ../intro/daytimetcpsrv.c */ 
#include "unp.h" 
#include <time.h> 

#define PORT 9999

int 
main(int argc, char **argv) 
{ 
	int listenfd, connfd; 
	struct sockaddr_in servaddr; 
	char buff[MAXLINE]; 
	time_t ticks;
	int n;
	size_t nbytes;
 
	listenfd = Socket(AF_INET, SOCK_STREAM, 0); 
 
	bzero(&servaddr, sizeof(servaddr)); 
	servaddr.sin_family = AF_INET; 
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY); 
	servaddr.sin_port = htons(PORT); /* daytime server */ 
 
	Bind(listenfd, (SA *) &servaddr, sizeof(servaddr)); 
 
	Listen(listenfd, LISTENQ); 
 
	for ( ; ; ) { 
		connfd = Accept(listenfd, (SA *) NULL, NULL); 
 
		ticks = time(NULL); 
		snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks));
		nbytes = strlen(buff);
		for (n = 0; n < nbytes; n++) {
			Write(connfd, buff+n, 1);
		}
		Close(connfd); 
	} 
} 
