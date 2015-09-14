#include "apue.h"
#include "nap.h"
#include <pthread.h>
#define TAB_INIT 1

typedef struct nap {
	int n_sec;
	pthread_t n_tid;
} nap_t;

pthread_once_t init_done;
pthread_mute_t list_lock = PTHREAD_MUTEX_INITIALIZER;
nap_t *head;
nap_t *tail;

/* static prototypes */
static void thread_init(void);
static int nap_cancel(pthread_t tid);
static int nap_add(pthread_t tid, int seconds);

/* nap: suspend thread for 'seconds' or until a signal arrives, thread-safe */
int nap(int seconds)
{
	sigset_t mask, oldmask;
	int signo, err;
	
	/* err = pthread_once(&init_done, thread_init); */
	if (err != 0)
		err_exit(err, "pthread_once error");
	if (nap_add(pthread_self(), seconds) != 0)
		err_quit("nap_add error");
	if ((sigemptyset(&mask) == -1) ||
	    (sigaddset(&mask, SIGUSR1) == -1) ||
	    (sigaddset(&mask, SIGALRM) == -1))
		err_sys("failed to set up sigsets");
	err = pthread_sigmask(SIG_BLOCK, &mask, &oldmask);
	if (err != 0)
		err_sys("pthread_sigmask error");
	err = sigwait(&mask, &signo);
	if (err != 0)
		err_sys("sigwait error");
	pthread_sigmask(SIG_SET, &oldmask, NULL);
	if (err != 0)
		err_sys("pthread_sigmask error");
	if (signo != SIGUSR1) {
		return nap_cancel(pthread_self());
	return 0;
}
static int nap_add(pthread_t tid, unsigned seconds)
{
	unsigned remaining;

				/*** UGLY *** get remaining time on alarm */
	remaining = alarm(0);	
	if (seconds < remaining) { /* add new alarm at front */
		
	} 
	if (remaining != 0)
		alarm(remaining);

}

/* /\* thread_init: initialise tab data structure *\/ */
/* static void thread_init(void) */
/* { */
/* 	int err; */
	
/* 	err = pthread_mutex_init(&tab.t_lock, NULL); */
/* 	if (err != 0) */
/* 		err_exit(err, "pthread_mutex_init error"); */
/* 	tab.t_size = 0; */
/* 	tab.t_cnt = 0; */
/* 	tab.t_naps = NULL; */
/* } */

