#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
/*
 * Exercise 3.2
 */
static int cat(int infd, int outfd);
static int our_dup2(int filedes, int filedes2);

/* duplicate fd and write stdin to stdout */
int main(void)
{
	char *file = "steal.txt";
	int fd;

	if ((fd = open(file, O_WRONLY)) == -1) {
		fprintf(stderr, "main: open failed\n");
		return (1);
	}
	if (our_dup2(fd, STDOUT_FILENO) == -1) {
		fprintf(stderr, "dup2 error\n");
		return 1;
	}
	cat(STDIN_FILENO, STDOUT_FILENO);
	return 0;
}

/* cat: write infd to outfd */
static int cat(int infd, int outfd)
{
	char buf[BUFSIZ];
	int n;	

	while ((n = read(infd, buf, BUFSIZ)) > 0)
		if (write(outfd, buf, n) < 0)
			fprintf(stderr, "write failed\n");
	if (n < 0)
		fprintf(stderr, "read failed\n");
	return n;
}

/* our_dup2: implement dup2 */
static int our_dup2(int filedes, int filedes2)
{
/*
 * No idea how to implement this without calling fcntl?
 */
}
