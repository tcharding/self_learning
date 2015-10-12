#include "unp.h"

#define PORT 9999

int main(int argc, char *argv[])
{
	int sockfd;
	struct sockaddr_in6 addr;
	char *host;
	int on = 1;
	
	if (argc != 2)
		err_quit("Usage: %s <host>");
	host = argv[1];

	sockfd = Socket(AF_INET6, SOCK_STREAM, 0);

	Setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

	bzero(&addr, sizeof(addr));
	addr.sin6_family = AF_INET6;
	addr.sin6_port = htons(PORT);
	inet_pton(AF_INET, host, &addr.sin6_addr.s6_addr);

	if (bind(sockfd, (SA *) &addr, sizeof(addr)) != 0)
		err_sys("bind error");

	if (listen(sockfd, LISTENQ) != 0)
		err_sys("listen error");
	pause();
}

	
