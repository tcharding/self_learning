/* Exercise 36.1 */
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/times.h>
#include "tlpi_hdr.h"

#define LOOP 1000

static void printRusage(int who);


int
main(int argc, char *argv[]) {
	pid_t childPid;
	int i, j, dummy;
	struct tms tms;

	setbuf(stdout, NULL);

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
		break;
	case 0:			/* child */
		for (i = 0; i < LOOP; i++) {
				/* consume some system CPU time */
			times(&tms);
				/* consume some user CPU time */
			for (j = 0; j < LOOP*100; j++)
				dummy = dummy * j + i;
		}
		_exit(EXIT_SUCCESS);
	default:		/* parent */
		sleep(2);
		printf("before wait\n");
		printRusage(RUSAGE_CHILDREN);
		wait(NULL);
		printf("after wait\n");
		printRusage(RUSAGE_CHILDREN);
	}

	exit(EXIT_SUCCESS);
}

/* printRusage: print process resource usage */
static void
printRusage(int who)
{
	struct rusage usage;

	if (getrusage(who, &usage) == -1)
		errExit("getrusage");

	switch (who) {
	case RUSAGE_SELF:
		printf("self: ");
		break;
	case RUSAGE_CHILDREN:
		printf("children: ");
		break;
	default:
		errExit("unknown 'who'");
		break;
	}
	printf("\n utime : %ld sec %ld usec\n",
	       usage.ru_utime.tv_sec, usage.ru_utime.tv_usec);
	printf("stime : %ld sec %ld usec\n",
	       usage.ru_stime.tv_sec, usage.ru_stime.tv_usec);
	printf("\n");
}
