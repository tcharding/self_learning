/* Exercise 34.3 */
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	pid_t childPid;

	setbuf(stdout, NULL);

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		if (setpgid(0, 0) == -1)
			errExit("setpgid");
		sleep(1);
		printf("child pid: %ld gid: %ld\n", (long) getpid(), (long) getpgid(0));

		if (setsid() < 0)
			printf("setsid failded");
		else
			printf("setsid succeeded");
		
		_exit(EXIT_SUCCESS);
	default:
		sleep(2);
		break;
	}

	exit(EXIT_SUCCESS);
}
