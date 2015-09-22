/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago 
 * modified main function to fork
*/
#include "apue.h"
#include <netdb.h>
#include <errno.h>
#include <syslog.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/wait.h>

#define QLEN 10

#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 256
#endif

extern int initserver(int, const struct sockaddr *, socklen_t, int);

static void serve(int sockfd)
{
	int clfd, status;
	pid_t pid;

	set_cloexec(sockfd);
	for ( ; ; ) {
		if ((clfd = accept(sockfd, NULL, NULL)) < 0) {
			syslog(LOG_ERR, "ruptimed: accept error: %s",
			       strerror(errno));
			exit(1);
		}
		if ((pid = fork()) < 0) {
			syslog(LOG_ERR, "ruptimed: fork error: %s",
			       strerror(errno));
			exit(1);			
		} else if (pid == 0) { /* child */
			if (dup2(clfd, STDOUT_FILENO) != STDOUT_FILENO ||
			    dup2(clfd, STDIN_FILENO) != STDIN_FILENO) {
				syslog(LOG_ERR, "ruptimed: unexpected error: %s",
				       strerror(errno));
				exit(1);
			}
			close(clfd);
			execl("/usr/bin/uptime", "uptime", (char *)0);
			syslog(LOG_ERR, "ruptimed: unexpected return from exec: %s",
				       strerror(errno));			
		} else {	/* parent */
			close(clfd);
			waitpid(pid, &status, 0);
		}
	}
}

int main(int argc, char *argv[])
{
	struct addrinfo *ailist, *aip;
	struct addrinfo hint;
	int sockfd, err, n;
	char *host;
	pid_t pid;
	
	if (argc != 1)
		err_quit("Usage: ruptimed");
	if ((n = sysconf(_SC_HOST_NAME_MAX)) < 0)
		n = HOST_NAME_MAX; /* best guess */
	host = Malloc(n);
	if (gethostname(host, n) < 0)
		err_sys("gethostname erro");
	daemonize("ruptimed");
	bzero(&hint, sizeof(hint));
	hint.ai_flags = AI_CANONNAME;
	hint.ai_socktype = SOCK_STREAM;
	hint.ai_canonname = NULL;
	hint.ai_addr = NULL;
	hint.ai_next = NULL;
	if ((err = getaddrinfo(host, "ruptime", &hint, &ailist)) != 0) {
		
	}
	for (aip = ailist; aip != NULL; aip = aip->ai_next) {
		if ((pid = fork() < 0)) {
			err_sys("fork error");
		} else if (pid == 0) { /* child */
			if ((sockfd = initserver(SOCK_STREAM, aip->ai_addr,
						 aip->ai_addrlen, QLEN)) >= 0) {
				serve(sockfd);
				exit(0);
			}
			else
				exit(1); 
		} 
	}
}
