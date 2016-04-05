/* Exercise 36.3 */
#include <sys/resource.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static void printRlimit(int resource);
static void setRlimitSoft(int resource, int soft);


int
main(int argc, char *argv[]) {
	int fd;

	setbuf(stdout, NULL);
	

	printRlimit(RLIMIT_NOFILE);
	setRlimitSoft(RLIMIT_NOFILE, 2);
	printRlimit(RLIMIT_NOFILE);

	fd = open("/dev/zero", O_RDONLY);
	if (fd == -1) 
		printf("open failed: %s\n", strerror(errno));
	
	printRlimit(RLIMIT_NPROC);
	setRlimitSoft(RLIMIT_NPROC, 2);
	printRlimit(RLIMIT_NPROC);
	
	exit(EXIT_SUCCESS);
}

static void
printRlimit(int resource)
{
	struct rlimit rlim;

	bzero(&rlim, sizeof(rlim));
	if (getrlimit(resource, &rlim) == -1)
		errExit("getrlimit");
	
	printf("hard: %ld soft: %ld\n",
	       rlim.rlim_max, rlim.rlim_cur);
}

static void
setRlimitSoft(int resource, int soft)
{
	struct rlimit rlim;
	int err;

	if (getrlimit(resource, &rlim) == -1)
		errExit("getrlimit");

	rlim.rlim_cur = soft;

	errno = 0;
	err = setrlimit(resource, &rlim);
	if (err == -1) {
		printf("setrlimit failed");
		if (errno != 0)
			printf("%s", strerror(errno));
		printf("\n");
	}
}
