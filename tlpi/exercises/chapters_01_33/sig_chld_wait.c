/* Exercise 27.6 */
#include <signal.h>
#include <sys/wait.h>
#include "tlpi_hdr.h"

void handler(int sig);

int
main(int argc, char *argv[]) {
	pid_t childPid;
	struct sigaction sa;
	sigset_t blockSet, oldSet;

	setbuf(stdout, NULL);
	
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = handler;
	sa.sa_flags = 0;

	if (sigaction(SIGCHLD, &sa, NULL) == -1)
		errExit("sigaction");

	sigemptyset(&blockSet);
	sigaddset(&blockSet, SIGCHLD);
	if (sigprocmask(SIG_BLOCK, &blockSet, &oldSet) == -1)
		errExit("sigprocmask");

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		_exit(0);
	default:		/* parent */
		sleep(1);	/* race condition here */
	}
	printf("parent: waiting ...\n");
	wait(NULL);
	printf("parent: returned from wait\n");

	printf("parent: unblocking signal\n");
	if (sigprocmask(SIG_SETMASK, &oldSet, NULL) == -1)
		errExit("sigprocmask");

	printf("parent: exiting ...\n");

	exit(EXIT_SUCCESS);
}

void handler(int sig)
{
	psignal(sig, "caught: ");
}
