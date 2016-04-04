/* Exercise 20.4 */
#include <signal.h>
#include "tlpi_hdr.h"

#define BUF_SIZE 1024

static void handler(int sig);
static void installHandler(void);
static int ourSiginterrupt(int sig, int flag);
static void ourRead(void);

int
main(int argc, char *argv[]) {
	installHandler();
	ourRead();
	
	fprintf(stderr, "setting current process disposition to SA_RESTART\n");
/*	siginterrupt(SIGINT, 0); */
	if (ourSiginterrupt(SIGINT, 0))
		errExit("ourSiginterrupt");
	ourRead();
	
	exit(EXIT_SUCCESS);	
}

static void
handler(int sig)
{
	psignal(sig, "caught");
}

/* installHandler: install handler for SIGINT */
static void
installHandler(void)
{
	struct sigaction sa;

	bzero(&sa, sizeof(sa));
	sa.sa_handler = handler;

	fprintf(stderr, "setting disposition for SIGINT\n");

	if (sigaction(SIGINT, &sa, NULL) == -1)
		errExit("sigaction");
}

/* siginterrupt: set SA_RESTART */
static int
ourSiginterrupt(int sig, int flags)
{
	struct sigaction sa;

	bzero(&sa, sizeof(sa));

	if (sigaction(sig, NULL, &sa) == -1)
		return 1;

	if (flags == 0)
		sa.sa_flags |= SA_RESTART;
	else
		sa.sa_flags &= ~SA_RESTART;

	if (sigaction(sig, &sa, NULL) == -1)
		return 2;

	return 0;
}

/* ourRead: call blocking read() */
static void
ourRead(void)
{
	char buf[BUF_SIZE];
	ssize_t nread;

	fprintf(stderr, "executing read(stdin)\n");
	if ((nread = read(STDIN_FILENO, buf, BUF_SIZE) == -1))
		fprintf(stderr, "read returned: -1: %s\n", strerror(errno));
	else 
		fprintf(stderr, "read returned: %d\n", (int)nread);
}
