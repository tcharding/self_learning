#include "apue.h"

/* fork and have child create new session */
int main(void)
{
	pid_t pid;

	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid == 0) {	/* child */
		if (setsid() != getpid())
			err_sys("setsid error");
		if (getpgrp() == getpid())
			fprintf(stderr, "I am process group leader\n");
		else
			fprintf(stderr, "I am not process group leader\n");

		sleep(2);
		fprintf(stderr, "child: sid:%ld pid:%ld ppid:%ld\n",
			(long)getsid(0), (long)getpid(), (long)getppid());

	} else {		/* parent */
		sleep(2);
		fprintf(stderr, "parent: sid:%ld pid:%ld ppid:%ld\n",
			(long)getsid(0), (long)getpid(), (long)getppid());
	}
	return 0;
}
