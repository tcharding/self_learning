/* Exercise 29.1 */
#include <pthread.h>
#include "tlpi_hdr.h"

static void *threadfunc(void *arg);

int
main(int argc, char *argv[]) {
	pthread_t tid;
	int e;
	
	e = pthread_create(&tid, NULL, threadfunc, NULL);
	if (e != 0)
		errExitEN(e, "pthread_create");

	sleep(3);
	exit(EXIT_SUCCESS);
}

static void *
threadfunc(void *arg)
{
	int e;
	e = pthread_join(pthread_self(), NULL);
	if (e != 0)
		errExitEN(e, "pthread_join");

	return (void *)0;
}
