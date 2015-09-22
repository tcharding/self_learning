/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
#include "apue.h"
#include <sys/socket.h>

#define MAXSLEEP 128

/* portable connect with exponential backoff */
int connect_retry(int domain, int type, int protocol,
		  const struct sockaddr *arrd, socklen_t alen)
{
	int numsec, fd;

	for (numsec = 1; numsec <= MAXSLEEP; numsec <<= 1) {
		if ((fd = socket(domain, type, protocol)) < 0)
			return (-1);
		if (connect(fd, addr, alen) == 0) {
			/* connection accepted */
			return (fd);
		}
		close(fd);	/* BSD requires new socket if connect fails */

		/*
		 * Delay before trying again
		 */
		if (numsec <= MAXSLEEP/2)
			sleep(numsec);
	}
	return (-1);
}
