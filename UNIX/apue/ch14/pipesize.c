#include "apue.h"
#include <fcntl.h>
#include <linux/limits.h>


/* get pipesize */
int main(void)
{
	int fd[2];
	int cnt;

	cnt = 0;
	Pipe(fd);
	set_fl(fd[1], O_NONBLOCK);

	fprintf(stderr, "PIPE_BUF: %d\n", PIPE_BUF);
	errno = 0;
	while(write(fd[1], "a", 1) != -1)
		cnt++;
	if (errno == EAGAIN)
		fprintf(stderr, "call would block (EAGAIN). cnt: %d)\n",cnt);
	else
		fprintf(stderr, "got another error: %d %s\n", errno, strerror(errno));
	return 0;
}
