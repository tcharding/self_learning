#include "apue.h"
#include "barrier.h"
#include <pthread.h>

static pthread_mutex_t block = PTHREAD_MUTEX_INITIALISER;
	
/* */
void barrier_init(barrier_t *b, unsigned count)
{
	int err;
	
	*b = *(barrier_t *)(Malloc(sizeof(struct barrier)));
	b->tids = (pthread_t *)calloc((size_t)count, sizeof(pthread_t));
	if (b->tids == NULL)
		err_sys("calloc failed");
	err = pthread_mutex_init(b->lock);
	if (err != 0)
		err_exit(err, "pthread_mutex_init error");
	b->b_cnt = count;
	b->b_here = 0;
}

/* */
int barrier_wait(barrier_t *b)
{
	
	return 0;
}

/* */
void barrier_destroy(barrier_t *b)
{
	if (b != NULL) {
		free(b->tids);
		free(b);
	}
}
