/* Exercise 33.1 */
#include <signal.h>
#include <pthread.h>
#include "tlpi_hdr.h"
#include "signal_functions.h"

static void *threadFn(void *arg);

int
main(int argc, char *argv[]) {
	sigset_t blockMask;
	pthread_t tid;
	int err;

	sigfillset(&blockMask);
	if (sigprocmask(SIG_BLOCK, &blockMask, NULL) == -1)
		errExit("sigprocmask");

	err = pthread_create(&tid, NULL, threadFn, NULL);
	if (err != 0)
		errExitEN(err, "create");

	err = pthread_kill(tid, SIGINT);
	if (err != 0)
		errExitEN(err, "kill");

	err = pthread_create(&tid, NULL, threadFn, NULL);
	if (err != 0)
		errExitEN(err, "create");

	err = pthread_kill(tid, SIGQUIT);
	if (err != 0)
		errExitEN(err, "kill");

	sleep(5);
	exit(EXIT_SUCCESS);
}

static void *threadFn(void *arg)
{
	sigset_t pending;

	sleep(1);		/* wait for signals */
	if (sigpending(&pending) == -1)
		errExit("sigpending"); /* kills all threads */

	printSigset(stdout, "sig", &pending);
	return 0;
}

