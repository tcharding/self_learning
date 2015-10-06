/* Authors: W. R. Stevens, B. Fenner, A. M. Rudoff */ 
#include "unp.h" 

static void chargen(int sockfd); 

int main(int argc, char **argv) 
{ 
	int listenfd, connfd; 
	pid_t childpid; 
	socklen_t clilen; 
	struct sockaddr_in cliaddr, servaddr; 
 
	listenfd = Socket(AF_INET, SOCK_STREAM, 0); 
 
	bzero(&servaddr, sizeof(servaddr)); 
	servaddr.sin_family = AF_INET; 
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY); 
	servaddr.sin_port = htons(SERV_PORT); 
 
	Bind(listenfd, (SA *) &servaddr, sizeof(servaddr)); 
 
	Listen(listenfd, LISTENQ); 
 
	for ( ; ; ) { 
		clilen = sizeof(cliaddr); 
		connfd = Accept(listenfd, (SA *) &cliaddr, &clilen); 
 
		if ( (childpid = Fork()) == 0) { /* child process */ 
			Close(listenfd); /* close listening socket */ 
			chargen(connfd);
			exit(0); 
		} 
		Close(connfd); /* parent closes connected socket */ 
	} 
} 

char chars[] = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLM"
		"NOPQRSTUVWXYZ[\\]^_`abcdefgh";

#define BUFLEN sizeof(chars)
#define HALF_SEC 500000000L;	/* half a second in nanoseconds */
/* chargen server */
static void chargen(int sockfd)
{
	char buf[BUFLEN];
	char c;
	struct timespec ts;

	ts.tv_sec = 1;
	ts.tv_nsec = 0; /* HALF_SEC; */
	strcpy(buf, chars);	/* original msg string */
	for ( ; ; ) {
		c = buf[0];
		memmove(buf, buf+1, BUFLEN-1);
		buf[BUFLEN-1] = c;
		Writen(sockfd, buf, BUFLEN);
		if (nanosleep(&ts, NULL) == -1)
			err_sys("nanosleep error");
	}
}
