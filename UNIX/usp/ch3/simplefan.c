/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 3.2 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/* Fork a chain of processes */
int main(int argc, char *argv[])
{
	pid_t child = 0;
	int i, n;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s processes\n", argv[0]);
		return 1;
	}
	n = atoi(argv[1]);
	for (i = 0; i < n; i++)
		if ((child = fork()) <= 0)
			break;
	sleep(5);
	fprintf(stderr, "i:%d process ID:%ld parent ID:%ld child ID:%ld\n",
		i, (long)getpid(), (long)getppid(), (long)child);

	return 0;
}
