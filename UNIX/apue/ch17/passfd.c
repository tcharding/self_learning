#include "apue.h"
#include <fcntl.h>

int multi_send_fd(int fd, int fd_to_send);
int multi_recv_fd(int, ssize_t (*func) (int, const void *, size_t));

void print_offset(char *msg, int fd)
{
	off_t offset;

	offset = lseek(fd, 0, SEEK_CUR); /* get current offset */
	fprintf(stderr, "pid:%ld %s [offset:%ld]\n",
		(long)getpid(), msg, (long)offset);
}

/* Exercise 17.2 */
int main(void)
{
	int pid;
	int fd;			/* fd to send/receive */
	int pipefd[2];		/* full-duplex pipe (UNIX domain socket) */
	char *file = "Makefile";

	TELL_WAIT();
	if (fd_pipe(pipefd) == -1)
		err_sys("fd_pipe error");
	
	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid == 0) {	/* child */
		close(pipefd[0]);
		if ((fd = open(file, O_RDONLY)) == -1)
			err_sys("open file error");
		print_offset("child opened file", fd);
		if (multi_send_fd(pipefd[1], fd) == -1)
			err_sys("sendfd error");
		fprintf(stderr, "Child sent fd\n");
		TELL_PARENT();
		WAIT_PARENT();
				/* update the offset and print */
		lseek(fd, 10, SEEK_CUR);
		print_offset("child set offset +10", fd);
		TELL_PARENT();
		WAIT_PARENT();
		lseek(fd, 10, SEEK_CUR);
		print_offset("child set offset +10 again", fd);
		TELL_PARENT();
		fprintf(stderr, "child exiting\n");
	} else {		/* parent */
		WAIT_CHILD();
		close(pipefd[1]);
		fd = multi_recv_fd(pipefd[0], write);
		print_offset("parent got file descriptor\n", fd);
		TELL_CHILD(pid);
		WAIT_CHILD();
		print_offset("parent should have new offset +10\n", fd);
		TELL_CHILD(pid);
		WAIT_CHILD();
		print_offset("parent should have new offset +10 again\n", fd);
		fprintf(stderr, "parent exiting\n");
	}
	exit(0);
}
