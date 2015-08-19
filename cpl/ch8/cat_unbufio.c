#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>

#define BUFSIZE 1024

void catfd(int fd);

int main(int argc, char *argv[])
{
	int fd;
	char *prog, *file;

	prog = argv[0];
	if (argc == 1) {	
		catfd(STDIN_FILENO);
	} else {		/* concatenate arguments */
		while (--argc > 0) {
			file = *++argv;
			if ((fd = open(file, O_RDONLY, 0)) < 0) {
				fprintf(stderr, "%s could not open %s\n", prog, file);
				continue;
			}
			catfd(fd);
			if (close(fd) < 0)
				fprintf(stderr, "%s could not close %s\n", prog, file);
		}
	}
	return 0;
}

/* catfd: write fd to stdout */
void catfd(int fd)
{
	char buf[1];
	int nbytes;

	while ((nbytes = read(fd, buf, 1)) > 0)
		write(STDOUT_FILENO, buf, nbytes);
}
