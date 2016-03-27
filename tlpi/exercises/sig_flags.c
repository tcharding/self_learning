/* Exercise 20.3 */
#include <signal.h>
#include "tlpi_hdr.h"

#define SLEEP 2
#define COUNT 5

static void showUseSA_RESETHAND(void);
static void showUseSA_NODEFER(void);
void sigCatcher(int sig);

static volatile sig_atomic_t count;

int
main(int argc, char *argv[]) {

	showUseSA_NODEFER();
	showUseSA_RESETHAND();

	exit(EXIT_SUCCESS);
}

/* showUseSA_RESETHAND: example useage of flag SA_RESETHAND */
static void
showUseSA_RESETHAND(void)
{
	struct sigaction act, oldact;
	int i;
	
	bzero(&oldact, sizeof(oldact));
	bzero(&act, sizeof(act));
	act.sa_handler = sigCatcher;
	act.sa_flags = SA_RESETHAND;
	
	fprintf(stderr, "Installing signal handler with SA_NODEFER (SIGQUIT)");

	if (sigaction(SIGQUIT, &act, &oldact) == -1)
		errExit("sigaction");

	for (i = 0; i < COUNT; i++)
		sleep(1);

	fprintf(stderr, "resetting disposition\n");
	if (sigaction(SIGQUIT, &oldact, NULL) == -1)
		errExit("sigaction");
	
}

/* showUseSA_NODEFER: example useage of flag SA_NODEFER */
static void
showUseSA_NODEFER(void)
{
	struct sigaction act, oldact;
	int i;
	
	bzero(&oldact, sizeof(oldact));
	bzero(&act, sizeof(act));
	act.sa_handler = sigCatcher;
	act.sa_flags = SA_NODEFER;
	
	fprintf(stderr, "Installing signal handler with SA_NODEFER (SIGQUIT)");

	if (sigaction(SIGQUIT, &act, &oldact) == -1)
		errExit("sigaction");

	for (i = 0; i < COUNT; i++)
		sleep(1);

	fprintf(stderr, "resetting disposition\n");
	if (sigaction(SIGQUIT, &oldact, NULL) == -1)
		errExit("sigaction");
}

/* sigCatcher: */
void
sigCatcher(int sig)
{
	fprintf(stderr, "entered sigCatcer: %d\n", ++count);
	sleep(SLEEP);
	fprintf(stderr, "exiting sigCatcer\n");
}
