/* orginal file: ../names/daytimetcpsrv2.c */ 
#include "unp.h" 
#include <time.h> 
 
int 
main(int argc, char **argv) 
{ 
	int listenfd, connfd; 
	socklen_t len, addrlen; 
	char buff[MAXLINE]; 
	time_t ticks; 
	struct sockaddr_storage cliaddr; 
	size_t nbytes;
	int i;
	
	if (argc == 2) 
		listenfd = Tcp_listen(NULL, argv[1], &addrlen); 
	else if (argc == 3) 
		listenfd = Tcp_listen(argv[1], argv[2], &addrlen); 
	else 
		err_quit("usage: daytimetcpsrv2 [ <host> ] <service or port>"); 
 
	for ( ; ; ) { 
		len = sizeof(cliaddr); 
		connfd = Accept(listenfd, (SA *)&cliaddr, &len); 
		printf("connection from %s\n", Sock_ntop((SA *)&cliaddr, len)); 
 
		ticks = time(NULL); 
		snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks)); 
		nbytes = strlen(buff);
		for (i = 0; i < nbytes; i++) {
			Write(connfd, buff+i, 1); 
		}
		Close(connfd); 
	} 
} 
