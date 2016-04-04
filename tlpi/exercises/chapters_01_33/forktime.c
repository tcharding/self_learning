/* Exercise 28.1 */
#include <sys/wait.h>
#include "tlpi_hdr.h"

static void usage(const char *prog);
static void loop(int times, Boolean vforkFlag, Boolean execFlag);

int
main(int argc, char *argv[]) {
	Boolean execFlag, vforkFlag;
	int opt;
	int num;
	
	execFlag = vforkFlag = FALSE;

	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usage(argv[0]);

	while ((opt = getopt(argc, argv, "ve")) != -1) {
		switch (opt) {
		case 'v':
			vforkFlag = TRUE;
			break;
		case 'e':
			execFlag = TRUE;
			break;
		default: /* '?' */
			usage(argv[0]);
		}
	}

	if (optind+1 > argc) {
		eprintf("Error: optind: %d argc: %d\n", optind, argc);
		exit(EXIT_FAILURE);
	}

	num = getInt(argv[optind], GN_NONNEG, "num");

	loop(num, vforkFlag, execFlag);

	exit(EXIT_SUCCESS);
}

static void
usage(const char *prog)
{
	usageErr("%s [-v] [-e] num\n", prog);
}

/* loop: fork and conditionally exec on each loop iteration */
static void
loop(int times, Boolean vforkFlag, Boolean execFlag)
{
	int i;
	pid_t childPid;

	for (i = 0; i < times; i++) {
		if (vforkFlag)
			childPid = vfork();
		else
			childPid = fork();

		if (childPid == -1)
			errExit("fork: %d", i);
		else if (childPid == 0) { /* child */
			if (execFlag) {
				execl("/usr/bin/true", "true", NULL);
				errExit("exec");
			}
			_exit(0);
		} else {		  /* parent */
			wait(NULL);
		}
	}
}
