/* Exercise 26.1 */
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	pid_t childPid;

	setbuf(stdout, NULL);

	switch(childPid = fork()) {
	case -1:
		errExit("fork");
		break;
	case 0:			/* child */
		sleep(3);
		printf("child: getppid: %ld\n", (long) getppid());
		exit(EXIT_SUCCESS);
	default:		/* parnet */
		_exit(EXIT_SUCCESS);
	}
}
