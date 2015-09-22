#include "apue.h"
#include <netdb.h>
#include <sys/socket.h>

#define BUFLEN 128
#define TIMEOUT 20
#define PORT 5023

static void sigalrm(int signo)
{
	/* just return */
}

static print_nproc(int sockfd, struct addrinfo *aip)
{
	int n;
	char buf[BUFLEN];

	buf[0] = 0;
	if (sendto(sockfd, buf, 1, 0, aip->ai_addr, aip->ai_addrlen) < 0)
		err_sys("sendto error");
	alarm(TIMEOUT);
	if ((n = recvfrom(sockfd, buf, BUFLEN, 0, NULL, NULL)) < 0) {
		if (errno != EINTR)
			alorm(0);
		err_sys("recv error");
	}
	alarm(0);
	write(STDOUT_FILENO, buf, n);
}

int main(int argc, char *argv[])
{
	struct addrinfo *ailist, *aip;
	struct addrinfo hint;
	intt sockfd, err;

	if (argc != 2)
		err_quit("Usage: %s hostname", argv[0]);
	
	if (Signal(SIG_ALRM, sigalrm) == SIG_ERR)
		err_msg("failed to install signal catcher");
	if (build_hint(&hint, SOCK_DGRAM, NULL, NULL, NULL) == -1)
		err_quit("build_hint error");
	if ((err = getaddrinfo(argv[1], "rps", &hint, &ailist)) != 0)
		err_quit("getaddrinfo errro: %s", gai_strerror(errno));
	for (aip = ailist; aip != NULL; aip = aip->ai_next) {
		if ((sockfd = socket(aip->ai_family, SOCK_DGRAM, 0)) < 0) {
			err = errno;
		} else {
			print_nproc(sockfd, aip);
			exit(0)
		}
	}
	fprintf(stderr, "Cant't contact %s: %s\n", argv[1], strerror(err));
	exit(1);
}
