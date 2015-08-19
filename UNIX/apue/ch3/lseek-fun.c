#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

/*
 * Exercise 3.6
 */

int main(void)
{
	char *file = "input.txt";
	char *s = "some random text\n";
	int fd, n, c;
	char buf[BUFSIZ];
	
	if ((fd = open(file, O_RDWR | O_APPEND)) == -1) {
		fprintf(stderr, "main: open error\n");
		return 1;
	}
				/* test append */
	if (write(fd, s, strlen(s)) < 0)
		fprintf(stderr, "main: write on file (O_APPEND) failed\n");

	if (lseek(fd, 0L, SEEK_SET) == -1) {
		fprintf(stderr, "main: lseek on file (O_APPEND) failed\n");
	}
				/* test append after seek */
	if (write(fd, s, strlen(s)) < 0)
		fprintf(stderr, "main: write on file (O_APPEND) failed\n");

	if (lseek(fd, 0L, SEEK_SET) == -1) {
		fprintf(stderr, "main: lseek on file (O_APPEND) failed\n");
	}

	while ((n = read(fd, buf, BUFSIZ)) > 0)
		if (write(STDOUT_FILENO, buf, n) < 0)
			fprintf(stderr, "write failed\n");
	if (n < 0)
		fprintf(stderr, "read failed\n");

	close(fd);
	return 0;

	fprintf(stderr, "now try to write to first line of file\n");

	if (lseek(fd, 0L, SEEK_SET) == -1) {
		fprintf(stderr, "main: lseek on file (O_APPEND) failed\n");
	}
	sprintf(buf, "New line writen in src file\n");
	if (write(fd, buf, strlen(buf)) < 0)
		fprintf(stderr, "write error");

	fprintf(stderr, "now dump file\n");
	if (lseek(fd, 0L, SEEK_SET) == -1) {
		fprintf(stderr, "main: lseek on file (O_APPEND) failed\n");
	}
	while ((n = read(fd, buf, BUFSIZ)) != -1)
		write(STDOUT_FILENO, buf, n);

	return 0;
}
