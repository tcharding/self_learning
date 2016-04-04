#include <sys/stat.h>
#include <fcntl.h>
#include <linux/limits.h>
#include "tlpi_hdr.h"


static int chdirAndBack(const char *path);
static int fChdirAndBack(int pathFd, int cwdFd);


int
main(int argc, char *argv[])
{
	int num;
	int i;
	char *path;
	int useFileDescriptor;

	if (argc < 3 || strcmp(argv[0], "--help") == 0)
		usageErr("%s num path FLAG", argv[0]);

	num = getInt(argv[1], GN_ANY_BASE, "num");
	path = argv[2];
	useFileDescriptor = getInt(argv[3], GN_ANY_BASE, "useFD");

	if (useFileDescriptor) {
		int pathFd, cwdFd;

		pathFd = open(path, O_RDONLY);
		if (pathFd == -1)
			errExit("open: %s", path);
		cwdFd = open(".", O_RDONLY);
		if (cwdFd == -1)
			errExit("open cwd");

		for (i = 0; i < num; i++) {
			if (fChdirAndBack(pathFd, cwdFd) == -1)
				exit(EXIT_FAILURE);
		}
	} else {

		for (i = 0; i < num; i++) {
			if (chdirAndBack(path) == -1)
				exit(EXIT_FAILURE);
		}

	}

	exit(EXIT_SUCCESS);
}

/* chdirAndBack: chdir to path and chdir back to cwd */
static int
chdirAndBack(const char *path)
{
	char cwd[PATH_MAX];

	if (getcwd(cwd, PATH_MAX) == NULL)
		return -1;

	if (chdir(path) == -1)
		errExit("chdir: %s", path);

	if (chdir(cwd) == -1)
		errExit("chdir: %s", cwd);

	return 0;
}

/* fChdirAndBack: chdir to path and chdir back to cwd using file descriptors */
static int
fChdirAndBack(int pathFd, int cwdFd)
{

	if (fchdir(pathFd) == -1)
		errExit("chdir: pathFd");

	if (fchdir(cwdFd) == -1)
		errExit("chdir cwdFd");

	return 0;
}
