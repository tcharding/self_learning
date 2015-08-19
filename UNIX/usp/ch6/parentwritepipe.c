/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 6.1 */
#include "tch.h"
#define BUFSIZE 10

int main(void)
{
	char bufin[BUFSIZE] = "empty";
	char bufout[] = "hello";
	int bytesin;
	pid_t pid;
	int fd[2];

	if (pipe(fd) == -1)
		err_sys("pipe error");
	bytesin = strlen(bufin);
	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid == 0) {	/* child */
		bytesin = read(fd[0], bufin, BUFSIZE);
	} else {		/* parent */
		write(fd[1], bufout, strlen(bufout)+1);
	}
	fprintf(stderr, "[%ld]:my bufin is {%.*s}, my bufout is {%s}\n",
		(long)getpid(), bytesin, bufin, bufout);
	return 0;
}
