/* Exercise */
#include <sys/xattr.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

#define PERMS S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH
#define FLAGS O_CREAT | O_WRONLY | O_APPEND

int
main(int argc, char *argv[]) {
	char *attrName;
	char *attrValue;
	char *file;
	int fd;
	
	if (argc < 4 || strcmp(argv[1], "--help") == 0)
		usageErr("%s attrName attrValue file", argv[0]);

	attrName = argv[1];
	attrValue = argv[2];
	file = argv[3];

	fd = open(file, O_WRONLY);
	if (fd == -1)
		errExit("open");

	if (fsetxattr(fd, attrName, attrValue, strlen(attrValue), 0) == -1)
		errExit("fsetxattr");

	if (close(fd) == -1)
		errExit("close");

	exit(EXIT_SUCCESS);
}
