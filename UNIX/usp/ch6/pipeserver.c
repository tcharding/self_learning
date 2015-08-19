#include "tch.h"
#include <errno.h>
#include <fcntl.h>
#include "restart.h"
#define FIFO_PERMS (S_IRWXU | S_IWGRP | S_IWOTH)

int main(int argc, char *argv[])
{
	int fd;

	if (argc != 2) 
		err_quit("Usage: %s fifoname > logfile\n", argv[0]);
	if ((mkfifo(argv[1], FIFO_PERMS) == -1) && (errno != EEXIST)) {
		err_sys("Server: mkfifo failed");
	}
				/* open read/write communication end point to pipe */
	if ((fd = open(argv[1], O_RDWR)) == -1)
		err_sys("Server: open failed");
	copyfile(fd, STDOUT_FILENO);
	return 1;
}
