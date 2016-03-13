/* Exercise 5.3 */
#include <fcntl.h>
#include "tlpi_hdr.h"

int open_file(const char *file, Boolean noAppend);
int write_bytes(int fd, int numBytes);

int
main(int argc, char *argv[])
{
	char *file;
	int numBytes;
	Boolean noAppend = FALSE;
	int fd;
	
	if (argc < 3 || argc > 4 || strcmp(argv[1], "--help") == 0)
		usageErr("%s filename num-bytes [x]", argv[0]);

	file = argv[1];
	numBytes = atoi(argv[2]);
	if (argc == 4) {
		if (strcmp(argv[3], "x") == 0)
			noAppend = TRUE;
		else
			errExit("unknown opiton: %s", argv[3]);
	}

	fd = open_file(file, noAppend);
	if (fd == -1)
		errExit("open_file");
	
	if (write_bytes(fd, numBytes) == -1)
		errExit("write_bytes");

	exit(EXIT_SUCCESS);
}

#define PERMS S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH
#define FLAGS O_CREAT | O_WRONLY
/*
 * open_file: open file with/without O_APPEND
 */
int open_file(const char *file, Boolean noAppend)
{
	int fd;
	int flags;

	if (noAppend)
		flags = FLAGS;
	else
		flags = FLAGS | O_APPEND;

	fd = open(file, flags, PERMS);
	if (fd == -1)
		return -1;

	return fd;
}
/*
 * write_bytes: Write numBytes one byte per write to fd
 */
int
write_bytes(int fd, int numBytes)
{
	int nwritten;
	int i;
	
	for (i = 0; i < numBytes; i++) {
		nwritten = write(fd, "x", 1);
		if (nwritten != 1)
			return -1;
	}

	return 0;
}
