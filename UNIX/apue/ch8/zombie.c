#include "apue.h"

/* create zombie */
int main(void)
{
	pid_t pid;

	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid == 0) {	/* child */
		exit(0);
	}
	system("ps -ljH");
				/* parent */
	sleep(10);
	exit(0);
}
