#include "tch.h"
#include "hardwaretimer.h"
#define MILLION 1000000L

enum {FALSE, TRUE};		/* 0 = FALSE, 1 = TRUE */

static sigset_t oldmask;

static long tvtomicro(struct timeval tv);

/* catchsetup: set up signal handler */
int catchsetup(Sigfunc *handler)
{
	struct sigaction act;

	act.sa_handler = handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	act.sa_flags |= SA_RESTART; /* restart interrupted system calls */
	if (sigaction(SIGALRM, &act, NULL) < 0)
		return -1;
	return 0;
}

/* is_interuptblocked: return TRUE if SIGALRM blocked, FALSE otherwise */
int is_interuptblocked(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&mask, SIGALRM) == TRUE)
		return TRUE;	/* already blocked */
	return FALSE;
}

/* blockinterrupt: block SIGALRM 
    save current process mask in oldmask */
int blockinterrupt(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&mask, SIGALRM) == TRUE)
		return TRUE;	/* already blocked */
	Sigemptyset(&mask);	/* re-use &mask */
	Sigaddset(&mask, SIGALRM);
	Sigprocmask(SIG_BLOCK, &mask, &oldmask);
	return 0;
}

/* unblockinterrupt: unblock SIGALRM
    save current process mask in oldmask */
void unblockinterrupt(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&oldmask, SIGALRM) == FALSE)
		return;		/* not currently blocked */
	Sigemptyset(&mask);	/* re-use &mask */
	Sigaddset(&mask, SIGALRM);
	Sigprocmask(SIG_UNBLOCK, &mask, &oldmask);
}

/* gethardwaretimer: get time remaining (microseconds), 0 if not running */
long gethardwaretimer(void)
{
	struct itimerval itv;

	if (getitimer(ITIMER_REAL, &itv) < 0)
		err_sys("getitimer error");
	return tvtomicro(itv.it_value);
}

/* sethardwaretimer: set timer using itimer */
void sethardwaretimer(long interval)
{
	struct itimerval itv;

	bzero(&itv.it_interval, sizeof(struct timeval)); /* no restart */
	itv.tv_value.tv_sec = interval / MILLION;
	itv.tv_value.tv_sec = interval % MILLION;

	if (setitimer(ITIMER_REAL, &itv, NULL) < 0)
		err_sys("setitimer error");
}
void stophardwaretimer(void)
{
	err_sys("stophardwaretimer not implemented");
}

/* waitforinterrupt: unblock SIGALRM and wait for signal */
void waitforinterrupt(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&mask, SIGALRM) == TRUE) {
		Sigdelset(&mask, SIGALRM); /* unblock SIGALRM */
	}
	(void)sigsuspend(&mask);
	fprintf(stderr, "waitforinterrupt: sigsuspend returned\n");
}

/* tvtomicro: return microsecond value of tv */
static long tvtomicro(struct timeval tv)
{
	return tv.tv_sec * MILLION + tv.tv_usec;
}
