#include "apue.h"

/* test tellwait record locking version */
int main(void)
{
	int pid;
	int counter;

	counter = 0;

	fprintf(stderr, "initial counter: %d\n", counter);
	tell_wait();
	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid > 0) {	/* parent */
		for ( ; ; ) {
			counter++;
			fprintf(stderr, "parent counter: %d\n", counter);
			sleep(1);
			tell_child(pid);
			wait_child();
		}
	} else {		/* child */
		for ( ; ; ) {
			wait_parent();
			counter++;
			fprintf(stderr, "child counter: %d\n", counter);
			tell_parent(); 
		}
	}
}

