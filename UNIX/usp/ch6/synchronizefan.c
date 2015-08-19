/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 6.2 */
#include "tch.h"
#include "restart.h"

/* Fork a chain of processes */
int main(int argc, char *argv[])
{
	char buf[] = "g";
	pid_t child = 0;
	int fd[2];
	int i, n;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s processes\n", argv[0]);
		return 1;
	}
	n = atoi(argv[1]);
	if (pipe(fd) == -1) {	/* create pipe */
		perror("Failed to create pipe");
		return 1;
	}
	for (i = 0; i < n; i++)	/* create fan of processes */
		if ((child = fork()) <= 0)
			break;
	if (child > 0) {	/* write synchronization chars to pipe */
		for (i = 0; i < n; i++)
			if (r_write(fd[1], buf, 1) != 1)
				perror("Failed to write sync characters");
	}
	if (r_read(fd[0], buf, 1) != 1) /* synchronize here */
		perror("Failed to read sync characters");
	
	fprintf(stderr, "i:%d process ID:%ld parent ID:%ld child ID:%ld\n",
		i, (long)getpid(), (long)getppid(), (long)child);

	return 0;
}
