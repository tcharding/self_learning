/* Exercise 5.2 */
#include <fcntl.h>
#include "tlpi_hdr.h"

#define PERMS S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH
#define FLAGS O_CREAT | O_WRONLY | O_APPEND

#ifndef BUF_SIZE        /* Allow "cc -D" to override definition */
#define BUF_SIZE 1024
#endif

int
main(int argc, char *argv[])
{
	int fd;
	char *file;
	off_t offset;
	char *output = "new string to append";
	ssize_t nwritten;
	
	if (argc < 2)
		usageErr("%s file\n", argv[0]);
	file = argv[1];

	fd = open(file, FLAGS, PERMS);
	if (fd == -1)
		errExit("opening file: %s", file);
	
	offset = lseek(fd, 0, SEEK_SET);
	if (offset != 0)
		errExit("lseek: failed seek to start of file: %d", offset);

	nwritten = write(fd, output, strlen(output));
	if (nwritten != strlen(output))
		errExit("write");

	if (close(fd) < 0)
		errExit("close");

	exit(EXIT_SUCCESS);
}
