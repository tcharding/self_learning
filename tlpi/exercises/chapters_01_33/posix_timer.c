/* Exercise 23.3 */
#define _POSIX_C_SOURCE 199309
#include <signal.h>
#include <time.h>
#include "tlpi_hdr.h"

void handler(int sig);

int
main(int argc, char *argv[]) {
	struct sigaction sa;
	timer_t timerid;
	struct itimerspec its;

	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s ", argv[0]);
	bzero(&its, sizeof(its));
	its.it_value.tv_sec = getInt(argv[1], GN_NONNEG, "sec");
	
	bzero(&timerid, sizeof(timerid));
	
	sa.sa_handler = handler;
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);

	sigaction(SIGALRM, &sa, NULL);

	if (timer_create(CLOCK_REALTIME, NULL, &timerid) == -1)
		errExit("timer_create");

	if (timer_settime(timerid, 0, &its, NULL) == -1)
		errExit("timer_settime");
	
	for (;;)
		sleep(1);

	exit(EXIT_SUCCESS);
}

void handler(int sig)
{
	psignal(sig, "caught");
}
