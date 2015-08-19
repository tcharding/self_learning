#include "socket.h"
#include <netinet/tcp.h>		/* for TCP_xxx defines */

void str_cli(FILE *fp, int sockfd);

int main(int argc, char *argv[])
{
	int sockfd;
	struct sockaddr_in servaddr;
	char *s;
	socklen_t len;
	struct sock_opts *ptr;
	
	if (argc == 1)
		s = "127.0.0.1";
	else
		s = argv[1];

	sockfd = Socket(AF_INET, SOCK_STREAM, 0);

	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(SERV_PORT);
	Inet_pton(AF_INET, s, &servaddr.sin_addr);

	print_opt_val(sockfd, "(before connect)");
	Connect(sockfd, (SA *) &servaddr, sizeof(servaddr));
	print_opt_val(sockfd, "(after connect)");
	
	str_cli(stdin, sockfd); /* do the work */

	exit(EXIT_SUCCESS);
}



