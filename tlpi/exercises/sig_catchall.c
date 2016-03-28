/* Exercise 22.2 */
#include <signal.h>
#include "tlpi_hdr.h"

static void handler(int sig);
static void loop(const char *msg, int num);

int
main(int argc, char *argv[]) {
	sigset_t blockMask, emptyMask;
	int num, n;

	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s num_loops", argv[0]);
	num = getInt(argv[1], 0, "num_loops");

	fprintf(stderr, "PID: %ld\n", (long)getpid());
	
	for (n = 1; n < NSIG; n++)          /* Same handler for all signals */
		(void) signal(n, handler);      /* Ignore errors */


	sigfillset(&blockMask);
	sigdelset(&blockMask, SIGINT);
        if (sigprocmask(SIG_SETMASK, &blockMask, NULL) == -1)
            errExit("sigprocmask");

	loop("looping", num);

	sigemptyset(&emptyMask);        /* Unblock all signals */
        if (sigprocmask(SIG_SETMASK, &emptyMask, NULL) == -1)
            errExit("sigprocmask");


	exit(EXIT_SUCCESS);
}


static void
handler(int sig)
{
	psignal(sig, "caught: ");
	if (sig == SIGINT)
		_exit(1);
}

static void
loop(const char *msg, int num)
{
	int i;
	
	for (i = 0; i < num; i++) {
		fprintf(stderr, "%s ...\n", msg);
		sleep(1);
	}
	
}
