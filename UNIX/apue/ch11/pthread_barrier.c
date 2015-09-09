#include "apue.h"
#include "pthread.h"
#include <pthread.h>
#define NTHR 3

static pthread_barrier_t b;

static void printids(const char *s);
/*@null@*/static void *th_fn(void *arg);
static void printids(const char *s);

/* test barrier */
int main(void)
{
	int i;
	pthread_t tid;
	int retval;

	tid = 0;
	srand48((long)getpid());
	(void)pthread_barrier_init(&b, NULL, NTHR+1);
	fprintf(stderr, "Parent creating %d threads\n", NTHR);
	for (i = 0; i < NTHR; i++)
		Pthread_create(&tid, NULL, th_fn, (void *)0);
	retval = pthread_barrier_wait(&b);
	fprintf(stderr, "parent through barrier: tid 0x%lx retval: %d\n",
		(unsigned long)pthread_self(), retval);
	sleep(1);
	exit(EXIT_SUCCESS);
}

/*@null@*/static void *th_fn(void *arg)
{
	struct timespec tv;
	int retval;
	
	bzero(&tv, sizeof(struct timespec));
	printids("thread starting:");
	tv.tv_sec = (int)(drand48() * 5.0);
	nanosleep(&tv, NULL);
	retval = pthread_barrier_wait(&b);
	if (retval == PTHREAD_BARRIER_SERIAL_THREAD)
		fprintf(stderr, "I'm the master thread: tid 0x%lx retval: %d\n",
		(unsigned long)pthread_self(), retval);
	else
		fprintf(stderr, "thread throug barrier: tid 0x%lx retval: %d\n",
		(unsigned long)pthread_self(), retval);
		
	return (void *)0;
}

static void printids(const char *s)
{
	pthread_t tid;

	tid = pthread_self();
	fprintf(stderr, "%s tid %lu (0x%lx)\n", s,
	       (unsigned long)tid, (unsigned long)tid);
}
