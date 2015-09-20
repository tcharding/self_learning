#include "apue.h"
#include <fcntl.h>

#include <sys/shm.h>
#include <sys/ipc.h>

#define NLOOPS 1000
#define SHM_SIZE sizeof(long)	
#define SHM_MODE 0600		/* user read/write */

static int update(long *ptr)
{
	return ((*ptr)++);	/* return value before increment */
}

/* increment counter in shared memory segment; XSI shared memory version */ 
int main(void)
{
	int shmid;
	int i, counter;
	pid_t pid;
	void *area;

	if ((shmid = shmget(IPC_PRIVATE, SHM_SIZE, SHM_MODE)) < 0)
		err_sys("msgget error");
	if ((area = shmat(shmid, 0, 0)) == (void *)-1)
		err_sys("shmat error");
	bzero(area, SHM_SIZE);	/* zero memory segment */
	
	TELL_WAIT();

	if ((pid = Fork()) > 0) { /* parent */
		for (i = 0; i < NLOOPS; i += 2) {
			if ((counter = update((long *)area)) != i)
				err_quit("parent: expected %d, got %d", i, counter);
			TELL_CHILD(pid);
			WAIT_CHILD();
		}
	} else {		/* child */
		for (i = 1; i < NLOOPS + 1; i += 2) {
			WAIT_PARENT();
			if ((counter = update((long *)area)) != i)
				err_quit("child: expected %d, got %d", i, counter);
			TELL_PARENT();
		}
	}
	if (shmctl(shmid, IPC_RMID, 0) < 0)
		err_sys("shmctl error");
	exit(0);
}
