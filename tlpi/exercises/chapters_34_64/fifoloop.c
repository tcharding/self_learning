/* Exercise 35.3 */
#define _GNU_SOURCE
#include <sched.h>
#include <sys/times.h>
#include "tlpi_hdr.h"

#define PRIORITY 50
#define QUARTES_PER_SECOND 4
#define QUARTES_PER_3_SECONDS 12


int
main(int argc, char *argv[]) {
	struct sched_param param;
	struct tms tms;
	clock_t cpuTime, start, t;
	int nquarters;		/* number of quarter seconds passed */
	int quarters;
	cpu_set_t set;
	pid_t childPid;
	
	alarm(5);

	setbuf(stdout, NULL);

	CPU_ZERO(&set);
	CPU_SET(0, &set);
	if (sched_setaffinity(getpid(), sizeof(cpu_set_t), &set) == -1)
		errExit("setaffinity");
	
	param.sched_priority = PRIORITY;
	if (sched_setscheduler(getpid(), SCHED_FIFO, &param) == -1)
			errExit("setscheduler");

	if ((childPid = fork()) == -1)
		errExit("fork");

				/* both fall through */
	cpuTime = 0;
	nquarters = 0;
	start = times(&tms);
	for (;;) {
		t = times(&tms) - start;

		quarters = t / 25;
/*		printf("t: %ld cupTime: %ld quartes: %d\n",
		(long) t, (long) cpuTime, quarters); */

		if (quarters > nquarters) {
			printf("pid: %ld cup Time: %ldms\n", (long) getpid(), cpuTime);
			nquarters = quarters;
		}
		if (nquarters > 0 && nquarters % QUARTES_PER_SECOND == 0)
			if (sched_yield() == -1)
				errExit("sched_yield");

		if (nquarters > QUARTES_PER_3_SECONDS)
			_exit(EXIT_SUCCESS);
	}

}

