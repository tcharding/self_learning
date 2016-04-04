/* Exercise 15.6 */
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static int chmod_arX(const char *file);

int
main(int argc, char *argv[])
{
	int i;
	char *file;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s file1 file2 ...");

	for (i = 1; i < argc; i++) {
		file = argv[i];
		chmod_arX(file);
	}

	exit(EXIT_SUCCESS);
}

/* do equivelent to: chmod a+rX file1 file2 ... */
static int
chmod_arX(const char *file)
{
	struct stat sb;
	mode_t mode;
	
	if (stat(file, &sb) == -1)
		return 1;

	mode = sb.st_mode | S_IRUSR | S_IRGRP | S_IROTH;
	if (sb.st_mode & S_IXUSR ||
	    sb.st_mode & S_IXGRP ||
	    sb.st_mode & S_IXOTH ||
	    S_ISDIR(sb.st_mode))
		mode = mode | S_IXUSR | S_IXGRP | S_IXOTH;

	if (chmod(file, mode) == -1)
		return 2;

	return 0;
}
