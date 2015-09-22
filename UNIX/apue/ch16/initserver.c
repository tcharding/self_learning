/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
#include "apue.h"
#include <sys/socket.h>
#include <errno.h>

int initserver(int type, const struct sockaddr *addr, socklen_t alen, int qlen)
{
	int fd, err;
	int reuse = 1;

	if ((fd = socket(addr->sa_family, type, 0)) < 0)
		return (-1);
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(int)) < 0)
		goto errout;
	if (bind(fd, addr, alen) < 0)
		goto errout;
	if (type == SOCK_STREAM || SOCK_SEQPACKET)
		if (listen(fd, qlen) < 0)
			goto errout;
	return (fd);

errout:
	err = errno;
	(void)close(fd);
	errno = err;
	return (-1);
}
