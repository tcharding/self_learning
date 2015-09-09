#include "apue.h"
#include <pthread.h>

static void *thr_fn1(void *arg);
static void *thr_fn2(void *arg);
static void printids(const char *s);

int main(void)
{
	int err;
	pthread_t tid1, tid2;

	bzero(&tid1, sizeof(pthread_t));
	bzero(&tid2, sizeof(pthread_t));
	err = pthread_create(&tid1, NULL, thr_fn1, NULL);
	if (err != 0)
		err_exit(err, "failed to create thread 1");
	/* err = pthread_join(tid1, (void *)&fp); */
	/* if (err != 0) */
	/* 	err_exit(err, "failed to join with thread 1"); */
	/* (void)sleep(1); */
	err = pthread_create(&tid2, NULL, thr_fn2, NULL);
	if (err != 0)
		err_exit(err, "failed to create thread 2");
	(void)sleep(1);
	printids("parent:  ");
	exit(0);
}

static void *thr_fn1(void *arg)
{	
	printids("thread 1:");
	pthread_exit((void *)0);
}

static void *thr_fn2(void *arg)
{
	printids("thread 2:");
	pthread_exit((void *)0);
}

static void printids(const char *s)
{
	pthread_t tid;

	tid = pthread_self();
	printf("%s tid %lu (0x%lx)\n", s,
	       (unsigned long)tid, (unsigned long)tid);
}
