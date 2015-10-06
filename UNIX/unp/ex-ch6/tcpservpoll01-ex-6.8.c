/* orginal file: ../tcpcliserv/tcpservpoll01.c */ 
/* include fig01 */ 
#include "unp.h" 
#include <limits.h> /* for OPEN_MAX */ 

#define OPEN_MAX_GUESS 255
#define OPEN_MAX_BIG 4096;	/* kernel hard limit */
#ifdef OPEN_MAX
static int openmax = OPEN_MAX;
#else
static int openmax = 0;
#endif
 
int 
main(int argc, char **argv) 
{ 
	int i, maxi, listenfd, connfd, sockfd; 
	int nready; 
	ssize_t n; 
	char buf[MAXLINE]; 
	socklen_t clilen; 
	struct pollfd *client;
	struct sockaddr_in cliaddr, servaddr; 

	fprintf(stderr, "pre: openmax: %d\n", openmax);
	if (openmax == -1) {	/* OPEN_MAX unsupported */
		openmax = OPEN_MAX_GUESS;
	} else if (openmax == 0) {	/* use sysconf */
		errno = 0;
		if ((openmax = sysconf(_SC_OPEN_MAX)) < 0) {
			if (errno != 0)
				err_sys("openmax error");
				/* it is indeterminate */
			openmax = OPEN_MAX_BIG; /* but we need a value */
		} 
	}
	client = Calloc(openmax, sizeof(struct pollfd));
	fprintf(stderr, "post: openmax: %d\n", openmax);
	
	listenfd = Socket(AF_INET, SOCK_STREAM, 0); 
 
	bzero(&servaddr, sizeof(servaddr)); 
	servaddr.sin_family = AF_INET; 
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY); 
	servaddr.sin_port = htons(SERV_PORT); 
 
	Bind(listenfd, (SA *) &servaddr, sizeof(servaddr)); 
 
	Listen(listenfd, LISTENQ); 
 
	client[0].fd = listenfd; 
	client[0].events = POLLRDNORM; 
	for (i = 1; i < openmax; i++) 
		client[i].fd = -1; /* -1 indicates available entry */ 
	maxi = 0; /* max index into client[] array */ 
/* end fig01 */ 
 
/* include fig02 */ 
	for ( ; ; ) { 
		nready = Poll(client, maxi+1, INFTIM); 
 
		if (client[0].revents & POLLRDNORM) { /* new client connection */ 
			clilen = sizeof(cliaddr); 
			connfd = Accept(listenfd, (SA *) &cliaddr, &clilen); 
#ifdef NOTDEF 
			printf("new client: %s\n", Sock_ntop((SA *) &cliaddr, clilen)); 
#endif 
 
			for (i = 1; i < openmax; i++) 
				if (client[i].fd < 0) { 
					client[i].fd = connfd; /* save descriptor */ 
					break; 
				} 
			if (i == openmax) 
				err_quit("too many clients"); 
 
			client[i].events = POLLRDNORM; 
			if (i > maxi) 
				maxi = i; /* max index in client[] array */ 
 
			if (--nready <= 0) 
				continue; /* no more readable descriptors */ 
		} 
 
		for (i = 1; i <= maxi; i++) { /* check all clients for data */ 
			if ( (sockfd = client[i].fd) < 0) 
				continue; 
			if (client[i].revents & (POLLRDNORM | POLLERR)) { 
				if ( (n = read(sockfd, buf, MAXLINE)) < 0) { 
					if (errno == ECONNRESET) { 
						/*4connection reset by client */ 
#ifdef NOTDEF 
						printf("client[%d] aborted connection\n", i); 
#endif 
						Close(sockfd); 
						client[i].fd = -1; 
					} else 
						err_sys("read error"); 
				} else if (n == 0) { 
					/*4connection closed by client */ 
#ifdef NOTDEF 
					printf("client[%d] closed connection\n", i); 
#endif 
					Close(sockfd); 
					client[i].fd = -1; 
				} else 
					Writen(sockfd, buf, n); 
 
				if (--nready <= 0) 
					break; /* no more readable descriptors */ 
			} 
		} 
	} 
} 
/* end fig02 */ 
