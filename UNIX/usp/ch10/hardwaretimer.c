#include "tch.h"
#include "hardwaretimer.h"
#define MILLION 1000000L

static sigset_t oldmask;
enum {FALSE, TRUE};		/* 0 = FALSE, 1 = TRUE */

static long tvtomicro(struct timeval tv);

/* ht_init: set up signal handler */
int ht_init(Sigfunc *handler)
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

/* ht_isblocked: return TRUE if SIGALRM blocked, FALSE otherwise */
int ht_isblocked(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&mask, SIGALRM) == TRUE)
		return TRUE;	/* already blocked */
	return FALSE;
}

/* ht_block: block SIGALRM 
    save current process mask in oldmask */
int ht_block(void)
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

/* ht_unblock: unblock SIGALRM
    save current process mask in oldmask */
void ht_unblock(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&oldmask, SIGALRM) == FALSE)
		return;		/* not currently blocked */
	Sigemptyset(&mask);	/* re-use &mask */
	Sigaddset(&mask, SIGALRM);
	Sigprocmask(SIG_UNBLOCK, &mask, &oldmask);
}

/* ht_get: get time remaining in microseconds, 0 if not running */
long ht_get(void)
{
	struct itimerval itv;

	if (getitimer(ITIMER_REAL, &itv) < 0)
		err_sys("getitimer error");
	return tvtomicro(itv.it_value);
}

/* ht_set: set timer using itimer
    nanosec -> microsec, loss of resolution */
void ht_set(struct timespec *tp)
{
	struct itimerval itv;

	bzero(&itv.it_interval, sizeof(struct timeval)); /* no restart */
	itv.it_value.tv_sec = tp->tv_sec;
	itv.it_value.tv_usec = 0;
	/* itv.it_value.tv_sec = tp->tv_nsec % 1000; /\* nano to micro *\/ */

	if (setitimer(ITIMER_REAL, &itv, NULL) < 0)
		err_sys("setitimer error");
}
/* ht_stop: */
void ht_stop(void)
{
	err_sys("ht_stop not implemented");
}

/* ht_wait: unblock SIGALRM and wait for signal */
void ht_wait(void)
{
	sigset_t mask;

	Sigprocmask(0, NULL, &mask);
	if (Sigismember(&mask, SIGALRM) == TRUE) {
		Sigdelset(&mask, SIGALRM); /* unblock SIGALRM */
	}
	(void)sigsuspend(&mask);
}

/* tvtomicro: return microsecond value of tv */
static long tvtomicro(struct timeval tv)
{
	return tv.tv_sec * MILLION + tv.tv_usec;
}
