#include "apue.h"
#include <fcntl.h>

#define LENGTH 2
#define NPROC 20

/* Exercise 14.1 */
int main(void)
{
	char *file = "ex1.file";
	int fd, i;
	pid_t pid;

	fd = Open(file, O_RDWR | O_CREAT | O_TRUNC, FILE_MODE);
	Write(fd, "ab", LENGTH);
	if (lseek(fd, 0, SEEK_SET) != 0)
		err_sys("lseek error");
	TELL_WAIT();
	if ((pid = fork()) < 0)
		err_sys("fork error");
	else if (pid > 0) {	/* parent */
		WAIT_CHILD(); /* wait for child to get read lock */
		fprintf(stderr, "parent trying to get write lock...\n");
		while (write_lock(fd, 0, SEEK_SET, LENGTH) < 0)
			;
		fprintf(stderr, "parent got write lock\n");
		sleep(1);
		exit(0);
		
	} else {		/* child, get read lock */
		fprintf(stderr, "child getting initial read lock\n");
		if (read_lock(fd, 0, SEEK_SET, LENGTH) == -1)
			err_sys("read_lock (fcntl) error");
		TELL_PARENT();
				/* create chain of processes */
		for (i = 0; i < NPROC; i++) {
			if ((pid = fork()) == -1)
				err_sys("fork error");
			else if (pid > 0)
				break;
			else { /* child, get more read locks */
				fprintf(stderr, "child getting read lock\n");
				if (read_lock(fd, 0, SEEK_SET, LENGTH) == -1)
					err_sys("read_lock (fcntl) error");
				sleep(1);
			}
		}
		sleep(1);
		exit(0);	
	}
}
