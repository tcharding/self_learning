/* Exercise 20.4 */
#include <signal.h>
#include "tlpi_hdr.h"

#ifndef SIG_HOLD
#define SIG_HOLD 100
#endif

/* implement System V signal API */

typedef void (*sighandler_t)(int);

sighandler_t sigset(int sig, sighandler_t handler);
/* void (*sigset(int sig, void (*handler)(int)))(int); */
int sighold(int sig);
int sigrelse(int sig);
int sigignore(int sig);
int sigpause(int sig);

int
main(int argc, char *argv[]) {

	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s ", argv[0]);
	
}

sighandler_t
sigset(int sig, sighandler_t handler)
{
	sigset_t mask;
	
	sigemptyset(&mask);
	sigaddset(&mask, sig);

	if (handler == SIG_HOLD) {
		sigprocmask(SIG_BLOCK, &mask, NULL);
		return handler;
	}

	sigprocmask(SIG_UNBLOCK, &mask, NULL);
	
	if (handler == SIG_IGN || handler == SIG_DFL)
		return signal(sig, handler);

	return signal(sig, handler);

}

int
sighold(int sig)
{
	sigset_t mask;
	
	sigemptyset(&mask);
	sigaddset(&mask, sig);

	return sigprocmask(SIG_BLOCK, &mask, NULL);
}

int
sigrelse(int sig)
{
	sigset_t mask;
	
	sigemptyset(&mask);
	sigaddset(&mask, sig);

	return sigprocmask(SIG_UNBLOCK, &mask, NULL);
}

int
sigignore(int sig)
{
	struct sigaction sa;

	sa.sa_handler = SIG_IGN;
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);

	return sigaction(sig, &sa, NULL);
}

int
sigpause(int sig)
{
	sigset_t unblockSet, oldSet;
	
	sigemptyset(&unblockSet);
	sigaddset(&unblockSet, sig);

	sigprocmask(SIG_UNBLOCK, &unblockSet, &oldSet);

	sigdelset(&oldSet, sig);
	return sigsuspend(&oldSet);
}
