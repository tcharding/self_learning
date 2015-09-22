#include "apue.h"
#include <fcntl.h>

/* enable asynchronous I/O on sockfd */
int enable_sigio(int sockfd)
{
	int flags;
				/* establish socket ownership */
	if (fcntl(sockfd, F_SETOWN, getpid()) == -1)
		return (-1);
				/* turn on async I/O */
	if ((flags = fcntl(sockfd, F_GETFL)) < 0)
		return (-1);
	if (fcntl(sockfd, F_SETFL, flags | O_ASYNC) == -1)
		return (-1);
	return 0;
}
/* disable asynchronous I/O on sockfd */
int disable_sigio(int sockfd)
{
	int flags;
				/* establish socket ownership */
	if (fcntl(sockfd, F_SETOWN, getpid()) == -1)
		return (-1);
				/* turn off async I/O */
	if ((flags = fcntl(sockfd, F_GETFL)) < 0)
		return (-1);
	if (fcntl(sockfd, F_SETFL, (flags & ~O_ASYNC)) == -1)
		return (-1);
	return 0;
}
