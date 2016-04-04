/* Exercise 23.1 */
#include <sys/time.h>
#include "tlpi_hdr.h"

static unsigned int ourAlarm(unsigned int seconds);

/* test ourAlarm */
int
main(int argc, char *argv[]) {
	int sec;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s seconds\n", argv[0]);

	sec = getInt(argv[1], GN_NONNEG, "sec");

	ourAlarm(sec);

	for (;;)
		sleep(1);

	exit(EXIT_SUCCESS);
}

/* ourAlarm: alarm(3) implementation using settimer(3) */
static unsigned int
ourAlarm(unsigned int seconds)
{
	int err;
	struct itimerval new, old;

	bzero(&new, sizeof(new));
	bzero(&old, sizeof(old));

	new.it_value.tv_sec = seconds;

	err = setitimer(ITIMER_REAL, &new, &old);
	if (err == -1)
		errExit("settimer"); /* alarm does not return error? */

	return old.it_value.tv_sec; /* 0 if no previous alarm set */
}
