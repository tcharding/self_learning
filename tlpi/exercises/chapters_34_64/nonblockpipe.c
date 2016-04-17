/* Exercise 44.7 */
#include <fcntl.h>
#include <sys/wait.h>
#include "tlpi_hdr.h"

#define SLEEP 1

int
main(int argc, char *argv[]) {
	pid_t childPid;
	int fd;
	int nread, flags;
	int c = 'a';	
	char *fifo = "pipe";
	
	setbuf(stdout, NULL);

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		fd = open(fifo, O_WRONLY);
		if (fd == -1)
			errExit("child: open");
		printf("child: sleeping\n");
		sleep(SLEEP);
		printf("child: writing to pipe ... sleeping\n");
		write(fd, &c, 1);
		sleep(SLEEP);

		printf("child: writing to pipe again ... exiting\n");
		write(fd, &c, 1);
		_exit(EXIT_SUCCESS);
	default:
		break;
				/* parent falls through */
	}
	fd = open(fifo, O_RDONLY);
		if (fd == -1)
			errExit("parent: open");
	
	printf("parent: reading from pipe\n");
	nread = read(fd, &c, 1);
	if (nread != 1)
		errExit("parent: read error");

	printf("parent: setting non-blocking and reading again\n");

	if (close(fd) == -1)
		errExit("parent: close");
	if (open(fifo, O_RDONLY | O_NONBLOCK) == -1)
		errExit("parent: open");

	/* flags = fcntl(fd, F_GETFD); */
	/* flags |= O_NONBLOCK; */
	/* if (fcntl(fd, F_SETFD, flags) == -1) */
	/* 	errExit("fcntl set"); */

	/* /\* verify flags set correctly *\/ */
	/* flags = fcntl(fd, F_GETFD); */
	/* if (!(flags & O_NONBLOCK))  */
	/* 	fatal("failed to set O_NONBLOCK\n"); */
		
	nread = read(fd, &c, 1);
	printf("parent read returned: %d %s\n", errno, strerror(errno));

	wait(NULL);
	exit(EXIT_SUCCESS);
}

