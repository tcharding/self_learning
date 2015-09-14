#include "apue.h"
#include "nap.h"
#include <pthread.h>
#define TAB_INIT 1

enum {FALSE, TRUE};

typedef struct nap {
	int n_flag;		/* TRUE if nap is valid */
	int n_sec;
	pthread_t n_tid;
} nap_t;

struct tab {
	pthread_mutex_t t_lock;
	int t_size;
	int t_cnt;
	nap_t *t_naps;		/* array of nap_t's */
} tab;

pthread_once_t init_done;

/* static prototypes */
static void thread_init(void);
static int gettimeleft(pthread_t tid);
static int nap_add(pthread_t tid, int seconds);

/* nap: suspend thread for 'seconds' or until a signal arrives, thread-safe */
int nap(int seconds)
{
	sigset_t mask, oldmask;
	int signo, err;
	
	err = pthread_once(&init_done, thread_init);
	if (err != 0)
		err_exit(err, "pthread_once error");
	if (nap_add(pthread_self(), seconds) != 0)
		err_quit("nap_add error");
	if ((sigemptyset(&mask) == -1) ||
	    (sigaddset(&mask, SIGUSR1) == -1) ||
	    (pthread_sigmask(SIG_BLOCK, &mask, &oldmask) == -1))
		err_sys("failed to set up thread sig mask");
	err = sigwait(&mask, &signo);
	if (err != 0)
		err_sys("sigwait error");
	if (signo != SIGUSR1)
		return gettimeleft(pthread_self());
	return 0;
}

/* thread_init: initialise tab data structure */
static void thread_init(void)
{
	int err;
	
	err = pthread_mutex_init(&tab.t_lock, NULL);
	if (err != 0)
		err_exit(err, "pthread_mutex_init error");
	tab.t_size = 0;
	tab.t_cnt = 0;
	tab.t_naps = NULL;
}
/* gettimeleft: return time remaining to nap */
static int gettimeleft(pthread_t tid)
{
	return 0;
}
static int nap_add(pthread_t tid, int seconds)
{
	nap_t *np;
	size_t nmemb;

	if (tab.t_size == 0) {	/* first time through */
		nmemb = TAB_INIT; 
		tab.t_naps = calloc(nmemb, sizeof(struct nap));
		if (tab.t_naps == NULL)
			err_sys("calloc error");
		tab.t_size = nmemb;
		/* tab.t_cnt = 0; redundant (tab has static storage) */
	} else if (tab.t_cnt >= tab.t_size) { /* re-size */
		nap_t *tmp;
		nmemb = tab.t_size * 2;
		tmp = calloc(nmemb, sizeof(struct nap));
		if (tmp == NULL)
			err_sys("calloc error");
		tab.t_naps = tmp;
		tab.t_size = nmemb;
	}
	np = &tab.t_naps[tab.t_cnt];
	np->n_flag = TRUE;
	np->n_sec = seconds;
	np->n_tid = tid;
	tab.t_cnt++;
	return tab.t_cnt;
}
