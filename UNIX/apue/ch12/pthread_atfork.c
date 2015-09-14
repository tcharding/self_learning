#include "apue.h"
#include <pthread.h>

static pthread_mutex_t lock1 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t lock2 = PTHREAD_MUTEX_INITIALIZER;

static void prepare(void)
{
	printf("preparing locks...\n");
	Pthread_mutex_lock(&lock1);
	Pthread_mutex_lock(&lock2);
}

static void parent(void)
{
	printf("parent unlocking...\n");
	Pthread_mutex_unlock(&lock1);
	Pthread_mutex_unlock(&lock2);
}

static void child(void)
{
	printf("child unlocking...\n");
	Pthread_mutex_unlock(&lock1);
	Pthread_mutex_unlock(&lock2);
}

static void *thr_fn(void *arg)
{
	arg = (void *)0; /* quiet lint */
	printf("thread started...\n");
	(void)pause();
	return (void *)0;
}

int main(void)
{
	int err;
	pid_t pid;
	pthread_t tid;

	bzero(&tid, sizeof(pthread_t));
	if ((err = pthread_atfork(prepare, parent, child)) != 0)
		err_exit(err, "pthread_atfork error");
	Pthread_create(&tid, NULL, thr_fn, NULL);

	(void)sleep(2);
	printf("parent about to fork\n");

	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid == 0)	/* child */
		printf("child returned from fork\n");
	else 			/* parent */
		printf("parent returned from fork\n");
	exit (0);
}
