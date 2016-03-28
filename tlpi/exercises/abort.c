/* Exercise 21.1 */
#include <signal.h>
#include "tlpi_hdr.h"

static void handler(int sig);
static void installHandler(void);
static void ourAbort(void);

/* test abort */
int
main(int argc, char *argv[]) {
	
	installHandler();
	
	ourAbort();

	fprintf(stderr, "Error: should not get here\n");
	exit(EXIT_FAILURE);
}

static void
handler(int sig)
{
	psignal(sig, "caught");
}

/* installHandler: install handler for SIGABRT */
static void
installHandler(void)
{
	struct sigaction sa;

	bzero(&sa, sizeof(sa));
	sa.sa_handler = handler;

	fprintf(stderr, "setting disposition for SIGINT\n");

	if (sigaction(SIGABRT, &sa, NULL) == -1)
		errExit("sigaction");
}

/* ourAbort: implement abort(3) */
static void
ourAbort(void)
{
	struct sigaction sa;
	
	raise(SIGABRT);

	bzero(&sa, sizeof(sa));
	sa.sa_handler = SIG_DFL;
	if (sigaction(SIGABRT, &sa, NULL) == -1)
		errExit("sigaction");
	raise(SIGABRT);
}
