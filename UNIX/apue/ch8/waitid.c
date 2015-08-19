#include "apue.h"
#include <sys/wait.h>

int main(void)
{
	pid_t pid;
	siginfo_t siginfo;
	int status;

	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid == 0)
		exit(7);

	if (waitid(P_PID, pid, &siginfo, WEXITED | WNOWAIT) == -1)
		err_sys("waitid error");
	
	if (wait(&status) != pid)
		err_sys("wait error");
	if (status)
		pr_exit(status);

	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid == 0)
		abort();

	if (waitid(P_PID, pid, &siginfo, WEXITED | WNOWAIT) == -1)
		err_sys("waitid error");
	fprintf(stderr, "si_signo: %d si_errno: %d si_code: %d\n",
		siginfo.si_signo, siginfo.si_errno, siginfo.si_code);

	/* if (wait(&status) != pid) */
	/* 	err_sys("wait error"); */
	/* pr_exit(status); */
	
	/* if ((pid = fork()) < 0) */
	/* 	err_sys("fork error"); */
	/* else if (pid == 0) */
	/* 	status /= 0; */

	/* if (wait(&status) != pid) */
	/* 	err_sys("wait error"); */
	/* pr_exit(status); */

	exit(0);
}
