/* orginal file: ../intro/daytimetcpcli.c */ 
#include "unp.h" 
#include "netinet/tcp.h"

static void show_buff_szs(int sockfd, const char *type);

int main(int argc, char **argv) 
{ 
	int sockfd, n; 
	char recvline[MAXLINE + 1]; 
	struct sockaddr_in servaddr;
	int rcv_sz, mss_sz;
	socklen_t len;
 
	if (argc != 2) 
		err_quit("usage: a.out <IPaddress>"); 

	len = sizeof(int);
	rcv_sz = mss_sz = 0;
	if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) 
		err_sys("socket error"); 

	bzero(&servaddr, sizeof(servaddr)); 
	servaddr.sin_family = AF_INET; 
	servaddr.sin_port = htons(13); /* daytime server */ 
	if (inet_pton(AF_INET, argv[1], &servaddr.sin_addr) <= 0) 
		err_quit("inet_pton error for %s", argv[1]); 
	
	Getsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, &rcv_sz, &len);
	Getsockopt(sockfd, IPPROTO_TCP, TCP_MAXSEG, &mss_sz, &len);
	printf("RCVBUF: %d MSS: %d\n", rcv_sz, mss_sz);
	
	if (connect(sockfd, (SA *) &servaddr, sizeof(servaddr)) < 0) 
		err_sys("connect error"); 
 
	Getsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, &rcv_sz, &len);
	Getsockopt(sockfd, IPPROTO_TCP, TCP_MAXSEG, &mss_sz, &len);
	printf("RCVBUF: %d MSS: %d\n", rcv_sz, mss_sz);

	while ( (n = read(sockfd, recvline, MAXLINE)) > 0) { 
		recvline[n] = 0; /* null terminate */ 
		if (fputs(recvline, stdout) == EOF) 
			err_sys("fputs error"); 
	} 
	if (n < 0) 
		err_sys("read error"); 
 
	exit(0); 
} 
/* show snd/rcv buffer sizes for sockfd */
static void show_buff_szs(int sockfd, const char *type)
{
	int sz;
	socklen_t len;

	sz = 0;
	len = (socklen_t)sizeof(int);
	Getsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, &sz, &len);
	printf("%s socket receive buffer: %d\n", type, sz);
	Getsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &sz, &len);
	printf("%s socket send buffer: %d\n", type, sz);
}
