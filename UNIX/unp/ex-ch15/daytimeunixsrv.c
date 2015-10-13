/* orginal file: ../names/daytimetcpsrv2.c */ 
#include "unp.h" 
#include <time.h> 
 
int 
main(int argc, char **argv) 
{ 
	int listenfd, connfd; 
	socklen_t clilen;
	struct sockaddr_un cliaddr, servaddr;
	char buff[MAXLINE]; 
	time_t ticks; 
	size_t nbytes;
	int i;
	
	if (argc != 1) 
		err_quit("usage: daytimetcpsrv2 [ <host> ] <service or port>"); 

	listenfd = Socket(AF_UNIX, SOCK_STREAM, 0);

	unlink(UNIXSTR_PATH);
	bzero(&servaddr, sizeof(struct sockaddr_un));
	servaddr.sun_family = AF_UNIX;
	strcpy(servaddr.sun_path, UNIXSTR_PATH);
	Bind(listenfd, (SA *) &servaddr, SUN_LEN(&servaddr));
	Listen(listenfd, LISTENQ);
	
	for ( ; ; ) { 
		clilen = sizeof(cliaddr); 
		if ( (connfd = accept(listenfd, (SA *)&cliaddr, &clilen)) < 0) {
			if (errno == EINTR)
				continue;
			else
				err_sys("accept error");
		} 
		printf("connection from %s\n", Sock_ntop((SA *)&cliaddr, clilen)); 
 
		ticks = time(NULL); 
		snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks)); 
		nbytes = strlen(buff);

		for (i = 0; i < nbytes; i++) {
			if (send(connfd, buff+i, 1, MSG_EOR) < 0)
				err_sys("send error");
			/* Write(connfd, buff+i, 1);  */
		}
		Close(connfd); 
	} 
} 
