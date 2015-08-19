/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 3.1 */

/* modified to answer questions in section 3.8 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>


/* Fork a chain of processes */
int main(int argc, char *argv[])
{
	pid_t child = 0;
	int i, j, n, nchars;
	char buf[BUFSIZ];
	
	if (argc != 3) {
		fprintf(stderr, "Usage: %s processes nchars\n", argv[0]);
		return 1;
	}
	
	n = atoi(argv[1]);
	nchars = atoi(argv[2]);
	if (nchars > BUFSIZ) {
		fprintf(stderr, "nchars: %d too big, must be less than %d",
			nchars, BUFSIZ);
		return 1;
	}

	for (i = 0; i < n; i++)
		if ((child = fork()))
			break;
	wait(NULL);
	for (j = 0; j < nchars; j++)
		buf[j] = fgetc(stdin);
	buf[nchars] = '\0';
	fprintf(stderr, "pid:%ld buf:%s\n", (long)getpid(), buf);

	return 0;
}
