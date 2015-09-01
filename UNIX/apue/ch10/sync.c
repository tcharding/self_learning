#include "apue.h"
#define NUM 3

static void inc_counter(FILE *stream);

/* test process synchronisation functions lib/tellwait.c */
int main()
{
	char *file = "sync.db";
	FILE *stream;
	pid_t pid;
	int counter;
	int i;

	counter = 0;
	stream = Fopen(file, "w+");
	fprintf(stream, "%d", counter);

	TELL_WAIT();
	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid == 0) {	/* child */

		for (i = 0; i < NUM; i++) {
			WAIT_PARENT();
			inc_counter(stream);
			TELL_PARENT(getppid());
		}
	} else {		/* parent */

		for (i = 0; i < NUM; i++) {
			inc_counter(stream);
			TELL_CHILD(pid);
			WAIT_CHILD();

		}
	}
	Fclose(stream);
	return 0;
}

static void inc_counter(FILE *stream)
{
	int counter;

	counter = 0;
	if (fseek(stream, 0, 0) == -1)
		err_sys("cannot rewind stream");
	if (fscanf(stream, "%d", &counter) != 1) /* one item to match */
		err_sys("fscanf error");

	counter++;		/* increment counter */
	fprintf(stderr, "pid:%ld inc counter to: %d\n", (long)getpid(), counter);		
	if (fseek(stream, 0, 0) == -1)
		err_sys("cannot rewind stream");
	fprintf(stream, "%d", counter);	
}
