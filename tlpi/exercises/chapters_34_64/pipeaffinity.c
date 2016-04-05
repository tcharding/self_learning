/* Exercise 35.4 */
#define _GNU_SOURCE
#include <sched.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

#define BUF_SIZ 64		/* less than a cache line? */

enum {
	R = 0,			/* read */
	W = 1			/* write */
};

enum CPU {
	SAME,
	DIF,
	UNSPEC,
};

int
main(int argc, char *argv[]) {
	char *file;
	pid_t childPid;
	int fd;
	int pipefd[2];
	int nread, nwritten;
	char buf[BUF_SIZ];
	enum CPU cpu;
	cpu_set_t set;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s file [s|d]\n", argv[0]);

	file = argv[1];
	fd = open(file, O_RDONLY);
	if (fd == -1)
		errExit("open: %s", file);

	if (argc == 3) {
		if (strcmp(argv[2], "d") == 0) {
			cpu = DIF;
		} else if (strcmp(argv[2], "s") == 0) {
			cpu = SAME;
		} else
			errExit("unknown flag: %s", argv[2]);
	} else {
		cpu = UNSPEC;
	}
	
	if (pipe(pipefd) == -1)
		errExit("pipe");

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		if (close(pipefd[W]) == -1 ||
		    close(fd) == -1)
			errExit("child: double close failed");

		CPU_ZERO(&set);
		switch (cpu) {
		case SAME:	/* fall through */
		case DIF:
			CPU_SET(0, &set);
			if (sched_setaffinity(getpid(), sizeof(cpu_set_t), &set) == -1)
				errExit("setaffinity");
			break;
		case UNSPEC:
			;
		}

		while ((nread = read(pipefd[R], buf, BUF_SIZ)) > 0)
			;	/* just read the pipe */
		if (nread == -1)
			errExit("child: read pipe");
		if (close(pipefd[R]) == -1)
			errExit("child: close read end");

		_exit(EXIT_SUCCESS);
	default:		/* parent */
		if (close(pipefd[R]) == -1)
			errExit("parent: close read end");

		CPU_ZERO(&set);
		switch (cpu) {
		case SAME:
			CPU_SET(0, &set);
			if (sched_setaffinity(getpid(), sizeof(cpu_set_t), &set) == -1)
				errExit("setaffinity");
			break;
		case DIF:
			CPU_SET(1, &set);
			if (sched_setaffinity(getpid(), sizeof(cpu_set_t), &set) == -1)
				errExit("setaffinity");
			break;
		case UNSPEC:
			;
		}

		while ((nread = read(fd, buf, BUF_SIZ)) > 0) {
			nwritten = write(pipefd[W], buf, BUF_SIZ);
			if (nwritten != nread)
				errExit("parent: write to pipe");
		}
		if (nread == -1)
			errExit("parent: read fd");

		if (close(fd) == -1 ||
		    close(pipefd[W]) == -1)
			errExit("parent: double close");
	}

	exit(EXIT_SUCCESS);
}
