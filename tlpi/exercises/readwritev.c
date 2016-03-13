/* Exercise 5.7 */
#include <sys/stat.h>
#include <sys/uio.h>		/* readv/writev */
#include <fcntl.h>
#include "tlpi_hdr.h"

ssize_t my_readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t my_writev(int fd, const struct iovec *iov, int iovcnt);

	
/* test my_readv, my_writev */
int
main(int argc, char *argv[])
{
	int fd;
	struct iovec iov[3];
	struct stat exStruct;
	int x;
#define STR_SIZE 100
	char str[STR_SIZE];
	ssize_t numRead, totRequired;

	if (argc != 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s file\n", argv[0]);

	fd = open(argv[1], O_RDONLY);
	if (fd == -1)
		errExit("open");

	totRequired = 0;

	iov[0].iov_base = &exStruct;
	iov[0].iov_len = sizeof(struct stat);
	totRequired += iov[0].iov_len;
	
	iov[1].iov_base = &x;
	iov[1].iov_len = sizeof(x);
	totRequired += iov[1].iov_len;
	
	iov[2].iov_base = &str;
	iov[2].iov_len = STR_SIZE;
	totRequired += iov[2].iov_len;

	numRead = my_readv(fd, iov, 3);
	if (numRead == -1)
		errExit("readv");

	if (numRead < totRequired)
		errMsg("Read fewer bytes than required");

	printf("total bytes requested: %ld; bytes read: %ld\n",
	       (long) totRequired, (long) numRead);

	exit(EXIT_SUCCESS);
}

/* NOTE: this is not the same as readv since it is not atomic */
ssize_t
my_readv(int fd, const struct iovec *iov, int iovcnt)
{
	int i;
	int nread;
	ssize_t total = 0;
	
	for (i = 0; i < iovcnt; i++) {
		nread = read(fd, iov[i].iov_base, iov[i].iov_len);
		if (nread == -1)
			return ((ssize_t) -1);
		total += nread;
	}
	return total;
}

ssize_t
my_writev(int fd, const struct iovec *iov, int iovcnt)
{
	int i;
	int nwritten;
	ssize_t total = 0;

	for (i = 0; i < iovcnt; i++) {
		nwritten = write(fd, iov[i].iov_base, iov[i].iov_len);
		if (nwritten == -1)
			return ((ssize_t) -1);
		total += nwritten;
	}
	return total;
}
