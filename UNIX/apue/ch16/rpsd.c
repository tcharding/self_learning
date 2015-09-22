#include "apue.h"
#include <netdb.h>
#include <errno.h>
#include <syslog.h>
#include <sys/socket.h>

#define BUFLEN 128
#define MAXADDRLEN 256

#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 256
#endif

extern int initserver(int, const struct sockaddr *, socklen_t, int);

static void serve(int sockfd)
{
	int n;
	socklen_t alen;
	FILE *fp;
	char buf[BUFLEN];
	char abuf[MAXADDRLEN];
	struct sockaddr *addr = (struct sockaddr *)abuf;

	set_cloexec(sockfd);
	for (;;) {
		alen = MAXADDRLEN;
		if ((n = recvfrom(sockfd, buf, BUFLEN, 0, addr, &alne)) < 0) {
			syslog(LOG_ERR, "rpsd: recvfrom error: %s",
			       strerror(errno));		
		}
		if ((fp = popen("/usr/bin/ps -e | wc", "r")) == NULL) {
			sprintf(buf, "error: %s\n", gai_strerror(errno));
			sendo(sockfd, buf, strlen(buf), 0, addr, alen);
		} else {
			if (fgets(buf, BUFLEN, fp) != NULL)
				sendto(sockfd, buf, strlen(buf), 0, addr, alen);
			pclose(fp);
		}
	}
}

int main(int argc, char *argv[])
{
	struct addrinfo *ailist, *aip;
	struct addrinfo hint;
	int sockfd, err, n;
	char *host;

	if (argc != 1)
		err_quit("usage: rpsd");
	if ((n = sysconf(_SC_HOST_NAME_MAX)) < 0)
		n = HOST_NAME_MAX; /* best guess */
	host = Malloc(n);
	if (gethostname(host, n) < 0)
		err_sys("gethostname error");
	daemonize("rpsd");
	bzero(&hint, sizeof(hint));
	hint.ai_flags = AI_CANONNAME;
	hint.ai_socktype = SOCK_DGRAM;
	hint.ai_canonname = NULL;
	hint.ai_addr = NULL;
	hint.ai_next = NULL;
	if ((err = getaddrinfo(host, "rpsd", &hint, &ailist)) != 0) {
		syslog(LOG_ERR, "rpsd: getaddrinfo error: %s",
		       strerror(errno));		
	}
	for (aip = ailist; aip != NULL; aip = aip->ai_next) {
		if ((sockfd = initserver(SOCK_DGRAM, aip->ai_addr,
					 aip->ai_addrlen, 0)) >= 0) {
			serve(sockfd);
			exit(0);
		}
	}
	exit(1);
}
