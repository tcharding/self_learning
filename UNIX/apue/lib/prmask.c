#include "apue.h"
#include <errno.h>
/*
 * Macro implementation based on lib/pr_mask.c: apue 
 *  author: Tobin Harding
 */
#define O_IFMEMB(sigstr)		      \
	if (sigismember(&sigset, sigstr))     \
		printf(" #sigstr")	  

int signals[] = {		/* asm/signal.h */
	SIGHUP,
	SIGINT,
	SIGQUIT,
	SIGILL,
	SIGTRAP,
	SIGABRT,
	SIGIOT,
	SIGBUS,
	SIGFPE,
	SIGKILL,
	SIGUSR1,
	SIGSEGV,
	SIGUSR2,
	SIGPIPE,
	SIGALRM,
	SIGTERM,
	SIGSTKFLT,
	SIGCHLD,
	SIGCONT,
	SIGSTOP,
	SIGTSTP,
	SIGTTIN,
	SIGTTOU,
	SIGURG,
	SIGXCPU,
	SIGXFSZ,
	SIGVTALRM,
	SIGPROF,
	SIGWINCH,
	SIGIO,
	SIGPOLL,
	/* SIGLOST, */
	SIGPWR,
	SIGSYS,
	SIGUNUSED,
};
#define NSIGNALS (sizeof(signals) / sizeof(signals[0]))

void pr_mask(const char *str)
{
	sigset_t sigset;
	int errno_save;
	int i;

	errno_save = errno;	/* we can be called by signal handlers */
	if (sigprocmask(0, NULL, &sigset) < 0) {
		err_ret("sigprocmask error");
	} else {
		printf("%s", str);
		for (i = 0; i < NSIGNALS; i++)
			O_IFMEMB(signals[i]);
		printf("\n");
	}
	errno = errno_save;		/* restore errno */
}
