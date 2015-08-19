#include "socket.h"

void dg_cli(FILE *fp, int sockfd, const SA *pservaddr, socklen_t servlen);

int main(int argc, char *argv[])
{
	int sockfd;
	struct sockaddr_in servaddr;
	char *sip;					/* IP address string */

	sip = (argc == 1) ? "127.0.0.1" : argv[1];

	sockfd = Socket(AF_INET, SOCK_DGRAM, 0);

	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(SERV_PORT);
	Inet_pton(AF_INET, sip, &servaddr.sin_addr);
	
	dg_cli(stdin, sockfd, (SA *) &servaddr, sizeof(servaddr));

	exit(0);
}
