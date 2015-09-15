#include "apue.h"

static void dummy(int);

int main(void)
{
	sigset_t mask;
	
	pr_mask("start");
	if (signal(SIGALRM, dummy) == SIG_ERR)
		err_sys("failed to install signal catcher");
	pr_mask("after signal()");
	
	Sigprocmask(0, NULL, &mask);
	Sigaddset(&mask, SIGALRM);
	Sigprocmask(SIG_BLOCK, &mask, NULL);
	pr_mask("after sigprocmask()");
	return 0;
}

static void dummy(int signo)
{
	;
				/* just return */
}
