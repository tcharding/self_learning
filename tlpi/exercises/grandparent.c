/* Exercise 26.2 */
#include <sys/wait.h>
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	pid_t grandparent, parent, child;
	int i;

	grandparent = getpid();
	
	printf("grandparent: pid: %ld\n", (long) grandparent);

	switch (parent = fork()) {
	case -1:
		errExit("fork");
		break;
	case 0:			/* first child process (called parent!) */
		switch (child = fork()) {
		case -1:
			errExit("fork");
			break;
		case 0:		/* child (conceptually: grandchild) */
			for (i = 0; i < 5; i++) {
				sleep(1);
				printf("child: ppid: %ld\n", (long) getppid());
			}
			_exit(EXIT_SUCCESS);
		default:	/* parent */
			printf("parent exiting, bye.\n");
			_exit(EXIT_SUCCESS);
		}
	default:		/* grandparent */
		printf("grandparent: going to sleep\n");
		sleep(3);
		printf("grandparent: calling wait()\n");
		wait(NULL);
		printf("grandparent: wait returned\n");
		_exit(EXIT_SUCCESS);
	}
}
