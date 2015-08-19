#include "socket.h"

void sig_chld(int signo)
{
	pid_t pid;
	int stat;

	while ( (pid = waitpid(-1, &stat, WNOHANG)) > 0)
		printf("child %d terminateted\n", pid);
	return;
}
