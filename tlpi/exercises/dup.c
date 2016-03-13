/* Exercise 5.4 */
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

int my_dup(int fd);
int my_dup2(int oldfd, int newfd);

/* test dup and dup2 implementations */
int
main(void)
{
	int fd;
	char *out = "This is the output string!\n";
	int nwritten;

	/* fd = my_dup2(STDOUT_FILENO, 3); */
	fd = my_dup(STDOUT_FILENO);
	nwritten = write(fd, out, strlen(out));
	if (nwritten != strlen(out))
		errExit("write");

	exit(EXIT_SUCCESS);
}

int my_dup(int fd)
{
	return fcntl(fd, F_DUPFD, 0);
}

int my_dup2(int oldfd, int newfd)
{
	int err;
	
	if (newfd == oldfd)
		return newfd;

	err = close(newfd);
	if (err == -1 && errno != EBADF)
		return -1;

	return fcntl(oldfd, F_DUPFD, newfd);
}
