/* Exercise 20.2 */
#include <signal.h>
#include "tlpi_hdr.h"

static void setDisposition(void);
static void ignoreSignal(void);
static void handler(int);

int
main(int argc, char *argv[]) {
	int seconds;
	int i;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s seconds", argv[0]);
	seconds = getInt(argv[1], 0, "seconds");
	
	setDisposition();
	
	for (i = 0; i < seconds; i++)
		sleep(1);

	ignoreSignal();

	for (;;)
		pause();

	exit(EXIT_SUCCESS);
}

/* setDisposition: set the signal disposition for SIGQUIT */
static void
setDisposition(void)
{
	struct sigaction act;

	bzero(&act, sizeof(act));
	act.sa_handler = handler;

	if(sigaction(SIGQUIT, &act, NULL) == -1)
		errExit("sigaction");
}

/* ignoreSignal: set SIGQUIT disposition to SIG_IGN */
static void
ignoreSignal(void)
{
	struct sigaction act;

	bzero(&act, sizeof(act));
	act.sa_handler = SIG_IGN;

	fprintf(stderr, "setting disposition of SIGQUIT to SIG_IGN");
	if(sigaction(SIGQUIT, &act, NULL) == -1)
		errExit("sigaction");
	
}

/* handler: signal handler */
static void
handler(int sig)
{
	psignal(sig, "caught");
}
