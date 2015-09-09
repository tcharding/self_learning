#include "apue.h"
#include "pthread.h"
#include <pthread.h>
#define NTHREADS 3
/* 
 * Simulate multi-thread work que
 *  
 * Master thread adds jobs to que, worker threads remove and complete jobs.
nnnnn * Master pre-allocates jobs by including a thread ID tag with each job,
 * threads only complete jobs allocated to them.
 */
	
struct job {
	pthread_t j_tid;
	long j_work;
	struct job *j_next;
};

/* should these have internal linkage (static) ? */
struct job *jobq;
pthread_mutex_t qlock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t qready = PTHREAD_COND_INITIALIZER;

/* prototypes */
static void *thr_fn(void *arg);
static void printids(const char *s);
static struct job *getjob(void);

int main(void)
{
	int i;
	pthread_t tids[NTHREADS];
	long work;
	struct job *jp;
	struct timespec tv;

	for (i = 0; i < NTHREADS; i++) /* zero array */
		bzero(&tids[i], sizeof(pthread_t));

	srand48((long)getpid()); /* set seed */
	bzero(&tv, sizeof(struct timespec));
	tv.tv_sec = (time_t)1;	 /* configure tv for one second */
	tv.tv_nsec = 0L;
				/* create threads */
	for (i = 0; i < NTHREADS; i++) 
		Pthread_create(&tids[i], NULL, thr_fn, NULL);

				/* add jobs */
	for ( ; ; ) {
		work = lrand48();
		jp = Malloc(sizeof(struct job));
		jp->j_tid = tids[work % NTHREADS];
		jp->j_work = work;
		jp->j_next = NULL;
		Pthread_mutex_lock(&qlock);
		jp->j_next = jobq;
		jobq = jp;
		Pthread_mutex_unlock(&qlock);
		pthread_cond_signal(&qready);
		nanosleep(&tv, NULL);
	}

	exit(EXIT_FAILURE);	/* should not get here */
}

/* thr_fn: worker thread */
static void *thr_fn(void *arg)
{
	struct job *jp;
	struct timespec tv;

	bzero(&tv, sizeof(struct timespec));
	printids("");
	for ( ; ; ) {
		jp = getjob();
		if (jp != NULL) {
			fprintf(stderr, "TID: 0x%lx got job: %ld\n",
				pthread_self(), jp->j_work);
			tv.tv_nsec = jp->j_work;
			nanosleep(&tv, NULL);
			free(jp);
		} else {
			;	/* conditional sleep here */
		}
	}
	return (void *)1;	/* should not get here */
}
/* getjob: search jobq for job, return job or NULL if not found */
static struct job *getjob(void)
{
	struct job *cur, *prev;
	
	Pthread_mutex_lock(&qlock);
	while (jobq == NULL)
		pthread_cond_wait(&qready, &qlock);
				/* locate a job */
	if (jobq->j_tid == pthread_self()) { /* first job is ours */
		cur = jobq;
		jobq = cur->j_next;		  /* remove it */
	} else {
		prev = jobq, cur = jobq->j_next;
		while (cur != NULL) {
			if (cur->j_tid == pthread_self()) { /* found one */
				prev->j_next = cur->j_next; /* remove it */
				break;
			}
			prev = cur, cur = cur->j_next; /* move along one */
		}
	}
	Pthread_mutex_unlock(&qlock);
	return cur;
}

void enque_jobs(pthread_t tids[])
{
}

static void printids(const char *s)
{
	pthread_t tid;

	tid = pthread_self();
	fprintf(stderr, "%s tid %lu (0x%lx)\n", s,
	       (unsigned long)tid, (unsigned long)tid);
}
