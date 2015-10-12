/* orginal file: ../names/daytimeudpsrv2.c */ 
#include "unp.h" 
#include <time.h> 
 
int 
main(int argc, char **argv) 
{ 
	int sockfd; 
	ssize_t n; 
	char buff[MAXLINE]; 
	time_t ticks; 
	socklen_t len; 
	struct sockaddr_storage cliaddr; 
	struct addrinfo hints, *res;
	int on = 1;
	
	if (argc == 2) 
		sockfd = Udp_server(NULL, argv[1], NULL); 
	else if (argc == 3) {
		bzero(&hints, sizeof(struct addrinfo));
		hints.ai_flags = AI_PASSIVE;
		hints.ai_family = AF_UNSPEC;
		hints.ai_socktype = SOCK_DGRAM;

		if ( ( n = getaddrinfo(argv[1], argv[2], &hints, &res)) != 0)
			err_quit("udp_server error for %s, %s: %s",
				 argv[1], argv[2], gai_strerror(n));
	
		do {
			sockfd = socket(res->ai_family, res->ai_socktype,
					res->ai_protocol);
			if (sockfd < 0)
				continue; /* error - try next one */
			Setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR,
				   &on, sizeof(on));
			if (bind(sockfd, res->ai_addr, res->ai_addrlen) == 0)
				break; /* success */
			Close(sockfd);
		} while ( (res = res->ai_next) != NULL);

		if (res == NULL)
			err_sys("udp_server error");
	} else 
		err_quit("usage: daytimeudpsrv [ <host> ] <service or port>"); 
 
	for ( ; ; ) { 
		len = sizeof(cliaddr); 
		n = Recvfrom(sockfd, buff, MAXLINE, 0, (SA *)&cliaddr, &len); 
		printf("datagram from %s\n", Sock_ntop((SA *)&cliaddr, len)); 
 
		ticks = time(NULL); 
		snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks)); 
		Sendto(sockfd, buff, strlen(buff), 0, (SA *)&cliaddr, len); 
	} 
} 
