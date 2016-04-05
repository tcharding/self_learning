/* Exercise 36.3 */
#include <sys/resource.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	int fd;
	int err;
	struct rlimit rlim;

	setbuf(stdout, NULL);
	
	fd = open("/dev/zero", O_RDONLY);
	if (fd == -1)
		errExit("open /dev/zero");

	if (getrlimit(RLIMIT_NOFILE, &rlim) == -1)
		errExit("getrlimit");

	rlim.rlim_cur = 2;	/* one less than currently open */

	errno = 0;
	err = setrlimit(RLIMIT_NOFILE, &rlim);
	printf("setrlimit return: %d\n", err);
	if (errno != 0)
		printf("rlimit_nofile: %s\n", strerror(errno));
	else
		printf("setrlimit_nofile succeded\n");

	if (getrlimit(RLIMIT_NPROC, &rlim) == -1)
		errExit("getrlimit");

	rlim.rlim_cur = 2;	/* clearly less that currently */

	errno = 0;
	err = setrlimit(RLIMIT_NPROC, &rlim);
	printf("setrlimit return: %d\n", err);
	if (errno != 0)
		printf("rlimit_nproc: %s\n", strerror(errno));
	else
		printf("setrlimit_nproc succeded\n");

	
	exit(EXIT_SUCCESS);
}
