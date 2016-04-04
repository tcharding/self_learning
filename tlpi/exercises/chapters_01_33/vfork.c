/* Exercise 24.2 */
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	pid_t childPid;

	setbuf(stdout, NULL);

	printf("forking ...\n");

	switch(childPid = vfork()) {
	case -1:
		errExit("vfork"); break;
	case 0:			/* child */
		if (close(STDOUT_FILENO) == -1)
			errExit("failed to close stdout in child");
		printf("Child: this should not appear\n");
		_exit(0);
	default:		/* parent */
		printf("Parent: still able to write after child closed stdout!\n");
	}

	exit(EXIT_SUCCESS);
}
