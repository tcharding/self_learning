#include "unp.h"

/* print snd/rcv buffer default sizes (TCP, UDP, SCTP)*/
int main(void)
{
	int sockfd, i;
	struct sockaddr_in addr;
	int sz;
	socklen_t len;
	char *msg;

	sz = 0;
	len = (socklen_t)sizeof(int);

	sockfd = Socket(AF_INET, SOCK_DGRAM, 0);
				/* make msg too big for UDP */
	Getsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &sz, &len);
	sz *= 2;
	msg = Malloc(sz);
	bzero(msg, sz);
	
	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
	addr.sin_port = htons(SERV_PORT);

	for (i = 0; /* infinite */ ; i++) {
		if (sendto(sockfd, msg, i, 0, &addr, (socklen_t)sizeof(addr)) < 0)
			err_sys("sendto error, i: %d", i);
	}
	
	return (0);
}
