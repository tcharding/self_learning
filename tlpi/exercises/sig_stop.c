/* Exercise 22.0 */
#include <signal.h>
#include "tlpi_hdr.h"

#define LOOP 3

static void loop(const char *msg, int num);
static void handler(int sig);
static void establishHandler(void);


int
main(int argc, char *argv[]) {
	sigset_t blockMask, oldMask;
	
/*	if (argc < 2 || strcmp(argv[1], "--help") == 0)
	usageErr("%s ", argv[0]); */

	loop("initial", LOOP);

	establishHandler();
	loop("with handler", LOOP);

	sigemptyset(&blockMask);
	sigaddset(&blockMask, SIGCONT);
	sigprocmask(SIG_BLOCK, &blockMask, &oldMask);
	loop("blocked", LOOP);

	sigprocmask(SIG_SETMASK, &oldMask, NULL);
	loop("unblocked", LOOP);

	exit(EXIT_SUCCESS);
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

static void
establishHandler(void)
{
	struct sigaction sa;

	sa.sa_handler = handler;
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);

	if (sigaction(SIGCONT, &sa, NULL) == -1)
		errExit("sigaction");
}

static void
handler(int sig)
{
	psignal(sig, "caught: ");
}
