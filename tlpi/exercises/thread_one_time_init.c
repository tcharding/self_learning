/* Exercise 31.1 */
#include <pthread.h>
#include "tlpi_hdr.h"

/*
struct init_control {
	pthread_mutex_t *mutex;
	Boolean init;
};

int one_time_init(struct init_control *control, void (*fn)(void)) 
*/

/* 
   Can't get this working using a struct!
 */
int one_time_init(pthread_mutex_t *mutex, int *hasRun, void (*fn)(void));

/* one_time_init: implement pthread_once */
int one_time_init(pthread_mutex_t *mutex, int *hasRun, void (*fn)(void))
{
	int err;

	err = pthread_mutex_lock(mutex);
	if (err != 0)
		return 1;

	if (*hasRun == 0) {
		fn();
		*hasRun = 1;
	}

	err = pthread_mutex_unlock(mutex);
	if (err != 0)
		return 2;

	return 0;
}

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int hasRun = 0;

static void *threadFunc(void *arg);
static void init(void);

/* test implementation */
int
main(int argc, char *argv[]) {
	int threads = 10;
	int i, err;
	pthread_t tid;
	
	for (i = 0; i < threads; i++) {
		err = pthread_create(&tid, NULL, threadFunc, NULL);
		if (err != 0)
			errExit("create");
		fprintf(stderr, "thread: %ld started\n", (long) tid);
	}

	sleep(3);
	exit(EXIT_SUCCESS);
}


static void *
threadFunc(void *arg)
{
	pthread_detach(pthread_self());
	one_time_init(&mutex, &hasRun, init);

	return NULL;
}

static void
init(void)
{
	fprintf(stderr, "init function ran\n");
}
