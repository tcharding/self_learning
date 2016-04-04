/* Exercise 34.7 */
#include <signal.h>
#include "tlpi_hdr.h"

void handler(int sig);

int
main(int argc, char *argv[]) {
	pid_t childPid;
	struct sigaction sa;

	sa.sa_handler = handler;
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	if (sigaction(SIGTTIN, &sa, NULL) == -1)
		errExit("sigaction");
	
	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		sleep(1);
				/* now we are an orphan */
		printf("pid: %ld (we have handler installed for SIGTTIN)\n", (long) getpid());

		for (;;)
			pause(); /* wait for signals */

		exit(EXIT_SUCCESS);
	default:
		_exit(EXIT_SUCCESS);
	}
	
}

void handler(int sig)
{
	psignal(sig, "caught: ");
}
