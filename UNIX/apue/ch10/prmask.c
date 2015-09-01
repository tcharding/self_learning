#include "apue.h"
#include <errno.h>

static void myprmask(const char *str);
static int next_blocked(sigset_t *set);
/* 
 * We need a way to do this in a system independent manner 
 */

/* from /usr/include/bits/signum.h */
#define NUM_SIGNALS _NSIG	
char *signals[] = {
	NULL,
	"SIGHUP",
	"SIGINT",
	"SIGQUIT",
	"SIGILL",
	"SIGTRAP",
	"SIGABRT",
	"SIGIOT",
	"SIGBUS",
	"SIGFPE",
	"SIGKILL",
	"SIGUSR1",
	"SIGSEGV",
	"SIGUSR2",
	"SIGPIPE",
	"SIGALRM",
	"SIGTERM",
	"SIGSTKFLT",
	"SIGCLD",
	"SIGCHLD",
	"SIGCONT",
	"SIGSTOP",
	"SIGTSTP",
	"SIGTTIN",
	"SIGTTOU",
	"SIGURG",
	"SIGXCPU",
	"SIGXFSZ",
	"SIGVTALRM",
	"SIGPROF",
	"SIGWINCH",
	"SIGPOLL",
	"SIGIO",
	"SIGPWR",
	"SIGSYS",
	"SIGUNUSED",
	NULL,
};


static void myprmask(const char *str);

/* test myprmask */
int main()
{
	sigset_t newset, oldset;
	
	if (sigemptyset(&newset) < 0) {
		err_ret("sigemptyset error");
	} else {
		if (sigprocmask(SIG_SETMASK, &newset, &oldset) < 0) 
			err_ret("sigprocmask error");
		myprmask("empty set");
		msg("END");
		if (sigprocmask(SIG_SETMASK, &oldset, NULL) > 0)
			err_ret("sigprocmask error");
	}
	if (sigfillset(&newset) < 0) {
		err_ret("sigfilset error");
	} else {
		if (sigprocmask(SIG_SETMASK, &newset, &oldset) < 0) 
			err_ret("sigprocmask error");
		myprmask("full set");
		msg("END");
		if (sigprocmask(SIG_SETMASK, &oldset, NULL) > 0)
			err_ret("sigprocmask error");
	}
	msg("now SIGALRM, SIGBUS, SIGPIPE");
	if (sigemptyset(&newset) < 0) {
		err_ret("sigemptyset error");
	} else {
		if (sigaddset(&newset, SIGALRM) < 0)
			err_sys("sigaddset error");
		if (sigaddset(&newset, SIGBUS) < 0)
			err_sys("sigaddset error");
		if (sigaddset(&newset, SIGPIPE) < 0)
			err_sys("sigaddset error");
				
		if (sigprocmask(SIG_SETMASK, &newset, &oldset) < 0) 
			err_ret("sigprocmask error");
		myprmask("custom set");
		msg("END");
		if (sigprocmask(SIG_SETMASK, &oldset, NULL) < 0)
			err_ret("sigprocmask error");
	}
	return 0;
}


static void myprmask(const char *str)
{
	int i;
	sigset_t set;

	if (sigprocmask(0, NULL, &set) > 0)
		err_sys("sigprocmask error");
	
	fprintf(stderr, "%s ", str);
	while ((i = next_blocked((unsigned long *)&set)) != 0)
		fprintf(stderr, " %s", signals[i]);
	fprintf(stderr, "\n");
}

/* next_blocked: toggle rightmost 'on' bit and return index of it, else 0 */
static int next_blocked(unsigned long *set)
{
	unsigned long tmp;
	int i;
	long long mask;

	if (*set == (sigset_t)0)
		return 0;	/* no bits set */
	tmp = *set;
	mask = (long long)1;
	for (i = 0; i < 64; i++) {
		if (tmp && mask)
			break;
		mask == mask << 1;
	}
	if (i == 64)
		err_quit("error: loop completed"); /* should not get here */
	
	mask = (long long)1 << i; 
	*set = *set && ~(mask);	/* turn off bit */
	return i;
}
