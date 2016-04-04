/* Exercise 34.6 */
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	pid_t childPid;
	char c;
	int fd;
	int nread;

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		sleep(1);
				/* now we are an orphan */
		fd = open("/dev/tty", O_RDONLY);
		if (fd == -1)
			errExit("open");
		nread = read(fd, &c, 1);
		if (nread != 1)
			printf("read from controlling terminal failed with error: %s\n",
			       strerror(errno));
		else
			printf("read from controlling terminal succeded\n");
		exit(EXIT_SUCCESS);
	default:
		_exit(EXIT_SUCCESS);
	}
	
}
