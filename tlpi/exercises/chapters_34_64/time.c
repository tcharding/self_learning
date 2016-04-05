/* Exercise 36.2 */
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/times.h>
#include "tlpi_hdr.h"

#define LOOP 1000

static void printRusage(const char *cmd);


int
main(int argc, char *argv[]) {
	pid_t childPid;

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
		break;
	case 0:			/* child */
		execvp(argv[1], &argv[1]);
		errExit("execvp");
	default:		/* parent */
		wait(NULL);
		printRusage(argv[1]);
	}

	exit(EXIT_SUCCESS);
}

#define MICRO 1000000
/* printRusage: print process resource usage */
static void
printRusage(const char *cmd)
{
	float sysTime, userTime;
	
	struct rusage ru, cru;

	if (getrusage(RUSAGE_SELF, &ru) == -1)
		errExit("getrusage");
	if (getrusage(RUSAGE_CHILDREN, &cru) == -1)
		errExit("getrusage");

	printf("\n%s ", cmd);

	userTime = 0;
	userTime += ru.ru_utime.tv_sec;
	userTime += ru.ru_utime.tv_usec / (float) MICRO;
	userTime += cru.ru_utime.tv_sec;
	userTime += cru.ru_utime.tv_usec / (float) MICRO;
	printf("%1.2fs user ", userTime);

	sysTime = 0;
	sysTime += ru.ru_stime.tv_sec;
	sysTime += ru.ru_stime.tv_usec / MICRO;
	sysTime += cru.ru_stime.tv_sec;
	sysTime += cru.ru_stime.tv_usec / MICRO;
	printf("%1.2fs system ", sysTime);
	
	printf("%1.2fs total\n", userTime+sysTime);
}
