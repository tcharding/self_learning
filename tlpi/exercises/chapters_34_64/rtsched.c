/* Exercise 35.2 */
#include <sys/wait.h>
#include <sched.h>
#include "tlpi_hdr.h"

int
main(int argc, char *argv[]) {
	int childPid;
	int policy;
	struct sched_param sp;
	char **cmd;
	
	if (argc < 4 || strcmp(argv[1], "--help") == 0)
		usageErr("%s policy(f|r) priority command [arg ...]\n", argv[0]);

	if (strcmp(argv[1], "r") == 0)
		policy = SCHED_RR;
	else if (strcmp(argv[1], "f") == 0)
		policy = SCHED_FIFO;
	else
		errExit("unknow policy: %s", argv[1]);

	sp.sched_priority = getInt(argv[2], 0, "priority");
	if (sp.sched_priority < 1 || sp.sched_priority > 99) {
		fprintf(stderr, "priority out of range (1-99): %d\n", sp.sched_priority);
		exit(EXIT_FAILURE);
	}

	cmd = &argv[3];

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		if (sched_setscheduler(getpid(), policy, &sp) == -1)
			errExit("setscheduler");

		if (seteuid(getuid()) == -1)
			errExit("seteuid");

		execvp(*cmd, cmd);
		errExit("execl failed");
	default:		/* parent */
		wait(NULL);
	}

	exit(EXIT_SUCCESS);

}

