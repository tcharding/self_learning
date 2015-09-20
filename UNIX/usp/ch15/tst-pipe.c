#include "tch.h"
#include "pipe.h"
#include <fcntl.h>

/* test pipe implementation */
int main(void)
{
	pipe_t *p;
	pid_t pid;
	char buf[PIPE_BUF];
	int nbytes, written;
	
	p = pipe_open();
	bzero(buf, sizeof(buf));
	if ((pid = Fork()) == 0) { /* child */
		while ((nbytes = pipe_read(p, buf, PIPE_BUF)) > 0) {
			msg("tst(child): read from pipe %d bytes\n", nbytes);
			if ((written = write(STDOUT_FILENO, buf, nbytes)) == -1)
				err_sys("write error");
			msg("tst(child): wrote %d bytes to stdout\n", written);
		}
		if (nbytes == -1)
			err_quit("pipe_read error");
		msg("child closing pipe");
		if (pipe_close(p, 0) == -1)
			err_msg("pipe_close error");
	} else {		/* parent */
		while ((nbytes = (int)read(STDIN_FILENO, buf, PIPE_BUF)) > 0) {
			msg("tst(parent): read from stdin %d bytes\n", nbytes);
			if (pipe_write(p, buf, nbytes) == -1)
				err_quit("pipe write error");
		} 
		if (nbytes == -1)
			err_sys("read error");
		msg("parent closing write end");
		if (pipe_close(p, O_WRONLY) == -1)
			err_quit("pipe_close o_wronly error");
	}
	(void)sleep(2);
	exit(0);
}
