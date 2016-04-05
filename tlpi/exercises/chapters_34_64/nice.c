/* Exercise 35.1 */
#include <sys/resource.h>
#include <sys/wait.h>
#include "tlpi_hdr.h"

/* implement nice(1) */

static void showNice(void);
static void commandWithNiceInc(char **cmd, int inc);

int
main(int argc, char *argv[]) {
	int inc = 10;		/* default same as nice(1) */
	char **cmd;
		
	if (argc > 1 && strcmp(argv[1], "--help") == 0)
		usageErr("%s [option] [command [arg] ...]\n"
			 "option:\n"
			 "\t-n, --adjustment=N \t add integer N to niceness (default 10)\n"
			 "\t--help \t\t print this menu\n", argv[0]);

	if (argc == 1) {
		showNice();
		exit(EXIT_SUCCESS);
	}

	if (argv[1][0] == '-') {
		if (strcmp(argv[1], "-n") == 0) {
			inc = getInt(argv[2], 0, "increment");
			cmd = &argv[3];
		} else {
			char *ptr = strchr(argv[2], '=');
			inc = getInt(++ptr , 0, "increment");
			cmd = &argv[2];
		}
	} else {
		cmd = &argv[1];
	}

	commandWithNiceInc(cmd, inc);

	exit(EXIT_FAILURE);	/* shouldn't get here */
}

/* showNice: show nice value of current process */
static void
showNice(void)
{
	int priority;

	priority = getpriority(PRIO_PROCESS, 0);
	printf("%d\n", priority);
}

/* commandWithNice: run cmd with nice value adjusted by inc 
     calls exit() */
static void
commandWithNiceInc(char **cmd, int inc)
{
	int priority;
	pid_t childPid;
	int status;
	
	priority = getpriority(PRIO_PROCESS, 0);
	priority += inc;

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		errno = 0;
		if (setpriority(PRIO_PROCESS, 0, priority) == -1) 
			if (errno != 0)
				errExit("failed to set priority in child");

		execv(*cmd, cmd);
		errExit("execl failed");
	default:		/* parent */
		wait(&status);
	}

	exit(EXIT_SUCCESS);
}
