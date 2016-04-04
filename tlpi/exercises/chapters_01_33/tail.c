/* Exercise 13.5 */
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static void tail(int numLines, int fd);
static char *readWholeFile(int fd);
static char *nextLine(char *ptr);

int
main(int argc, char *argv[])
{
	int fd;
	int num = 10;
	char *file;

        if (argc < 2 || strcmp(argv[1], "--help") == 0) {
		usageErr("%s [-n num] file\n", argv[0]);
	} else if (argc == 2) {
		file = argv[1];
	} else if (argc == 4 || strcmp(argv[1], "-n") == 0) {
		num = getInt(argv[2], GN_ANY_BASE, "num");
		file = argv[3];
	} else {
		usageErr("%s [-n num] file argc: %d\n", argv[0], argc);
	}

	fd = open(file, O_RDONLY);
	if (fd == -1)
		errExit("open");

	tail(num, fd);

	if (close(fd) == -1)
		errExit("close");
	
	exit(EXIT_SUCCESS);
}

static void
tail(int numLines, int fd)
{
	char *buf;
	char *start, *ptr;
	int sofar;

	sofar = 0;
	buf = readWholeFile(fd);
	start = buf;

	for (ptr = buf; *ptr != '\0'; ptr++) {
		if (*ptr != '\n')
			continue;
		sofar++;
		if (sofar > numLines) {
			start = nextLine(start);
		}
	}
	printf("%s", start);
}


/* readWholeFile: allocates memory and reads in whole file, must be free'd */
static char *
readWholeFile(int fd)
{
	struct stat sb;
	char *buf;
	size_t len, nread;
	
	if (fstat(fd, &sb) == -1)
		errExit("fstat");

	len = sb.st_size;
	buf = malloc(len);
	if (buf == NULL)
		errExit("malloc");

	nread = read(fd, buf, len);
	if (nread != len)
		errExit("read error");
	
	return buf;
}

/* nextLine: returns pointer to start of next line */
static char *
nextLine(char *buf)
{
	char *ptr;

	for (ptr = buf; *ptr != '\n'; ptr++)
		;

	return ++ptr;
}
