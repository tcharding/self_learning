/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 4.19 */
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>

#define CREATE_FLAGS (O_WRONLY | O_CREAT | O_TRUNC)
#define CREATE_PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

int main(int argc, char *argv[])
{
	char buf[BUFSIZ];
	pid_t childpid = 0;
	int fd, i, n;
	char *infile, *prog;

	prog = argv[0];
	if (argc != 3) {
		fprintf(stderr, "Usage: %s process filename\n", prog);
		return 1;
	}

	infile = argv[2];
	
	n = atoi(argv[1]);
	for (i = 0; i < n; i++) 
		if ((childpid = fork()))
			break;
	if (childpid == -1) {
		perror("Failed to fork");
		return 1;
	}

	fd = open(infile, CREATE_FLAGS, CREATE_PERMS);
	if (fd < 0) {
		fprintf(stderr, "Failed to open file: %s", infile);
		return 1;
	}
				/* write twice to common log file */
	sprintf(buf, "i:%d process:%ld ", i, (long)getpid());
	write(fd, buf, strlen(buf));
	sleep(1);
	sprintf(buf, "parent:%ld child:%ld\n", (long)getppid(), (long)getpid());
	write(fd, buf, strlen(buf));
	return 0;
}

