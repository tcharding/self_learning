#include "apue.h"
#include <fcntl.h>
#include <sys/mman.h>

#define NLOOPS 1000
#define SIZE sizeof(long)	/* size of shared memory segment */
#define BYTES 1			/* advisory record lock */

static int update(long *ptr)
{
	return ((*ptr)++);	/* return value before increment */
}

/* increment counter in shared memory segment; mmap /dev/zero version */ 
int main(void)
{
	int fd0, fdlock, i, counter;
	pid_t pid;
	void *area;
	char *file_lock = "lock.file";

	if ((fd0 = open("/dev/zero", O_RDWR)) < 0)
		err_sys("open error");
	if ((area = mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
			 fd0, 0)) == MAP_FAILED)
		err_sys("mmap error");
	close(fd0);		/* can close /dev/zero now that it is mapped */

	if ((fdlock = open(file_lock, O_RDWR | O_CREAT | O_TRUNC, FILE_MODE)) < 0)
		err_sys("fdlock open error");

	if ((pid = Fork()) > 0) { /* parent */
		for (i = 0; i < NLOOPS; i += 2) {
			writew_lock(fdlock, 0, SEEK_CUR, BYTES);
			if ((counter = update((long *)area)) != i)
				err_quit("parent: expected %d, got %d", i, counter);
			un_lock(fdlock, 0, SEEK_CUR, BYTES);
		}
	} else {		/* child */
		for (i = 1; i < NLOOPS + 1; i += 2) {
			writew_lock(fdlock, 0, SEEK_CUR, BYTES);
			if ((counter = update((long *)area)) != i)
				err_quit("child: expected %d, got %d", i, counter);
			un_lock(fdlock, 0, SEEK_CUR, BYTES);
		}
	}
	exit(0);
}
