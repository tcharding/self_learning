#include "unp.h"

static void show_buff_szs(int sockfd, const char *type);

/* print snd/rcv buffer default sizes (TCP, UDP, SCTP)*/
int main(void)
{
	int sockfd;

	sockfd = Socket(AF_INET, SOCK_STREAM, 0);
	show_buff_szs(sockfd, "TCP");
	Close(sockfd);
	sockfd = Socket(AF_INET, SOCK_DGRAM, 0);
	show_buff_szs(sockfd, "UDP");
	Close(sockfd);
	sockfd = Socket(AF_INET, SOCK_SEQPACKET, IPPROTO_SCTP);
	show_buff_szs(sockfd, "SCTP");
	Close(sockfd);
	
	return (0);
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

