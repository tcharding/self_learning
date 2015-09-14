#include "apue.h"
#include "barrier.h"
#include "pthread.h"
#include <pthread.h>

sigset_t mask;

/* */
void barrier_init(barrier_t *b, unsigned count)
{
	int err;

	/* TODO: add ability to reuse barrier */

	*b = *(barrier_t *)(Malloc(sizeof(struct barrier)));
	b->b_tids = (pthread_t **)calloc((size_t)count, sizeof(pthread_t *));
	if (b->b_tids == NULL)
		err_sys("calloc failed");
	*b->b_tids = NULL;	/* null terminator */
	fprintf(stderr, "barrier_init here \n");
	err = pthread_mutex_init(&b->b_lock, NULL);
	if (err != 0)
		err_exit(err, "pthread_mutex_init error");
	fprintf(stderr, "barrier_init here \n");
	b->b_cnt = count;
	b->b_here = 0;
	fprintf(stderr, "barrier_init here \n");
	/* set up signal mask */
	sigemptyset(&mask);
	sigaddset(&mask, SIGUSR1);
	if ((err = pthread_sigmask(SIG_BLOCK, &mask, NULL)) != 0)
		err_exit(err, "SIG_BLOCK error");
	fprintf(stderr, "barrier_init complete\n");
}

/* barrier_wait: block if 'here' is less than 'count' */
int barrier_wait(barrier_t *b)
{
	int signo, err;
	pthread_t tid, **tidvp;
	
	Pthread_mutex_lock(&b->b_lock);
	b->b_here++;
				/* add tid to b->b_tids */
	for (tidvp = b->b_tids; *tidvp != NULL; tidvp++)
		;		/* find free slot */
	*tidvp = Malloc(sizeof(pthread_t *));
	**tidvp = pthread_self();
	
	if (b->b_here < b->b_cnt) {
		Pthread_mutex_unlock(&b->b_lock);
		err = sigwait(&mask, &signo);
		if (err != 0)
			err_exit(err, "sigwait error");
		if (signo == SIGUSR1)
			return 0;
		return 1;	/* unknown signal */
	}
				/* signal threads at barrier */
	for (tidvp = b->b_tids; *tidvp != NULL; tidvp++) {
		tid = **tidvp;
		if (tid != 0 && tid != pthread_self()) {
			err = pthread_kill(tid, SIGUSR1);
			if (err != 0)
				err_exit(err, "pthread_kill error");
		}
	}
	Pthread_mutex_unlock(&b->b_lock);
	return BARRIER_SERIAL_THREAD;
}

/* */
void barrier_destroy(barrier_t *b)
{
	pthread_t **tidvp;
	
	if (b != NULL) {
		for (tidvp = b->b_tids; *tidvp != NULL; tidvp++) {
			free(*tidvp);
		}
		free(b->b_tids);
		free(b);
	}
}
