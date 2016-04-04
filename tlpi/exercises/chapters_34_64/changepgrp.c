/* Exercise 34.2 */
#include <signal.h>
#include "tlpi_hdr.h"

static void
handler(int sig)
{
}

#define TESTSIG SIGUSR1

int
main(int argc, char *argv[]) {
	pid_t childPid;
	struct sigaction sa;
	sigset_t blockedMask;
	
	sa.sa_handler = handler;
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	if (sigaction(TESTSIG, &sa, NULL) == -1)
		errExit("sigaction");


	sigemptyset(&blockedMask);
	sigaddset(&blockedMask, TESTSIG);
	if (sigprocmask(SIG_SETMASK, &blockedMask, NULL) == -1)
		errExit("sigprocmask");

	
	switch(childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:
		if (sigwaitinfo(&blockedMask, NULL) == -1)
			errExit("sigwaitinfo");

		sleep(3);

		execl("/usr/bin/sleep", "sleep", "5", NULL);
		errExit("execl failed");
	default:
		if (setpgid(childPid, 0) == -1)
			errExit("setpgid failed before exec");

		if (kill(getppid(), TESTSIG) == -1)
			errExit("kill");

		sleep(1);	/* wait for exec to complete */

		if (setpgid(childPid, getpid()) == -1)
			errExit("setpgid failed before exec");

	}
}
