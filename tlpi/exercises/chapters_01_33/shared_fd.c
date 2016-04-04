/* Exersice 5.5 */
#include <fcntl.h>
#include "tlpi_hdr.h"

#define PERMS S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH
#define FLAGS O_CREAT | O_WRONLY | O_TRUNC

#define LOOP 10

void confirm_same_offset(int fd1, int fd2);
void confirm_same_flags(int fd1, int fd2);

int
main(void)
{
	char *file = "tfile";
	int fd1, fd2;
	
	fd1 = open(file, FLAGS, PERMS);
	if (fd1 == -1)
		errExit("open");

	fd2 = dup(fd1);

	confirm_same_offset(fd1, fd2);
	
	if (close(fd1) == -1)
		errExit("close fd1");

	if (close(fd2) == -1) {
		errExit("close fd2");
	}

	exit(EXIT_SUCCESS);
}

void
confirm_same_offset(int fd1, int fd2)
{
	int nwritten;
	int i, modi;
	char buf[] = "abcdefghijklmnopqrstuvwxyz";
	
	for (i = 0; i < LOOP; i++) {
		modi = i % sizeof(buf);
		nwritten = write(fd1, buf+modi, 1);
		if (nwritten != 1)
			errExit("write on byte: %d to fd1", i);
		nwritten = write(fd2, buf+modi, 1);
		if (nwritten != 1)
			errExit("write on byte: %d to fd2", i);
	}

}

void
confirm_same_flags(int fd1, int fd2)
{
	int flags1, flags2;

	flags1 = fcntl(fd1, F_GETFD);
	flags2 = fcntl(fd2, F_GETFD);

	if (flags1 == -1 || flags2 == -1)
		errExit("fcntl");
	
	if (flags1 != flags2)
		errExit("flags don't match");

	if (flags1 & O_CLOEXEC)
		errMsg("close on exec is already on!");
	
	if (fcntl(fd1, F_SETFD, flags1 | O_CLOEXEC) == -1)
		errExit("fcntl failed to set flags");

		flags1 = fcntl(fd1, F_GETFD);
	flags2 = fcntl(fd2, F_GETFD);

	if (flags1 == -1 || flags2 == -1)
		errExit("fcntl");
	
	if (flags1 != flags2)
		errExit("flags don't match after set");
}
