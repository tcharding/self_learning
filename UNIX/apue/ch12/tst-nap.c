#include "apue.h"
#include <pthread.h>

#define NTHR 4

static int times[] = { 0, 1, 2, 3};
static void *thr_fn(void *arg);

int nap(int seconds);		/* nap.c */

/* test nap implementation */
int main(void)
{
	int i;
	pthread_t tid;
	struct timespec tv;

	bzero(&tid, sizeof(tid));
	bzero(&tv, sizeof(tv));
	fprintf(stderr, "Testing nap with %d threads\n", NTHR);

	for (i = 0; i < NTHR; i++) {
		Pthread_create(&tid, NULL, thr_fn, (void *)(times + i));
	}
	fprintf(stderr, "Parent going to sleep indefinitely\n"); 
	tv.tv_sec = 1;
	tv.tv_nsec = 0;
	for ( ; ; )
		nanosleep(&tv, NULL);
}

static void *thr_fn(void *arg)
{
	int *t;

	t = (int *)arg;
	fprintf(stderr, "Thread %d starting\n", *t);
	(void)nap(*t);
	fprintf(stderr, "Thread %d woke up\n", *t);
	return (void *)0;
}
