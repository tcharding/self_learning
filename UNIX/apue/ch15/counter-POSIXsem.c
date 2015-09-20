#include "apue.h"
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <semaphore.h>

#define NLOOPS 3
#define SIZE sizeof(long)	/* size of shared memory segment */
#define SEM_MODE 0600

static int update(long *ptr)
{
	return ((*ptr)++);	/* return value before increment */
}

/* increment counter in shared memory segment; mmap /dev/zero version */ 
int main(void)
{
	int fd, i, counter;
	pid_t pid;
	void *area;
	char *sem_name = "/sem"; /* POSIX semaphore */
	sem_t *sem;
	int value;

	if ((fd = open("/dev/zero", O_RDWR)) < 0)
		err_sys("open error");
	if ((area = mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
			 fd, 0)) == MAP_FAILED)
		err_sys("mmap error");
	close(fd);		/* can close /dev/zero now that it is mapped */

				/* ? init value being ignored ? */
	sem = sem_open(sem_name, O_CREAT, S_IRUSR | S_IWUSR, 0); 
	
	if (sem == SEM_FAILED)
			err_sys("sem_open error");
	if (sem_post(sem) == -1) /* shouldn't need this ? */
				err_sys("sem_post");
	if (sem_getvalue(sem, &value) == -1)
		err_sys("sem_getvalue error");

	fprintf(stderr, "sem initial value: %d\n", value);

	if ((pid = Fork()) > 0) { /* parent */
		fprintf(stderr, "parent running\n");
		for (i = 0; i < NLOOPS; i++) {
			if (sem_wait(sem) == -1)
				err_sys("sem_wait");
			counter = update((long *)area);
			fprintf(stderr, "parent: counter: %d\n", counter);
			if (sem_post(sem) == -1)
				err_sys("sem_post");
		}
	} else {		/* child */
		fprintf(stderr, "child running\n");
		sem = sem_open(sem_name, 0);/* open existing semaphore */
		if (sem == SEM_FAILED)
			err_sys("child sem_open error");
		for (i = 0; i < NLOOPS; i++) {
			if (sem_wait(sem) == -1)
				err_sys("sem_wait");
			counter = update((long *)area);
			fprintf(stderr, "child: counter: %d\n", counter);
			if (sem_post(sem) == -1)
				err_sys("sem_post");
		}
	}
	sleep(2);
	/* if ((sem_close(sem) < 0) || */
	/*     (sem_unlink(sem_name) < 0)) */
	/* 		err_sys("failed to clean up semaphore"); */
	exit(0);
}
