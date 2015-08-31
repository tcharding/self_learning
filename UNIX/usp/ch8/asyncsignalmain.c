#include "tch.h"
#include "asyncmonitorsignal.h"
#include <fcntl.h>
#include <sys/stat.h>

#define BLKSIZE 1024
#define MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

static void dowork(void);

int main(int argc, char *argv[])
{
	char buf[BLKSIZE];
	int done = 0;
	int error;
	int fd1, fd2;

	if (argc != 3)
		err_quit("Usage: %s infile outfile", argv[0]);

	bzero(buf, sizeof(buf));
				/* open file descriptors for I/O */
	fd1 = Open(argv[1], O_RDONLY, 0);
	fd2 = Open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, MODE);

	if (initsignal(SIGRTMAX) == -1)
		err_sys("initsignal error");

	if (initread(fd1, fd2, SIGRTMAX, buf, BLKSIZE) == -1)
		err_sys("failed on first read");

	for ( ; ; ) {
		dowork();
		if (done == 0) {
			if ((done = getdone()) == 1) {
				if ((error = geterror()) != 0) {
					err_ret("Failed to copy file");
				} else {
					err_ret("Copy successful, %d bytes\n",
						getbytes());
				}
			}
		}
	}
}  

static void dowork(void)
{
	(void)sleep(1);
}
