#include "apue.h"

void fn(void);

int main(void)
{
	
	printf("in main, pid: %ld\n", (long)getpid());
	fn();
	printf("fn has returned pid is now: %ld\n", (long)getpid());
	return 0;
}

void fn(void)
{
	pid_t pid;
	
	if ((pid = vfork()) < 0) {
		err_sys("fork error");
	} else if (pid == 0) {	/* child */
		printf("child ,pid: %ld\n", (long)getpid());
		return;
	}
				/* parent */
	printf("parent, pid: %ld\n", (long)getpid());
	fprintf(stderr, "parent returning now\n");
	return;
}
