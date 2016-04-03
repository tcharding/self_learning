/* Exercise 31.2 see basename_r also */
#include <libgen.h>
#include <pthread.h>
#include <limits.h>
#include "tlpi_hdr.h"

/* use thread local data */

static __thread char buf[PATH_MAX];

char *dirname_r(const char *path);

char *
dirname_r(const char *path)
{
	char localPath[PATH_MAX];

	strcpy(localPath, path);
	strcpy(buf, dirname(localPath));

	return buf;
}

/* test implementation */
int
main(int argc, char *argv[]) {
	char *dirname;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s path\n", argv[0]);

	dirname = dirname_r(argv[1]);
	printf("%s\n", dirname);
	
	exit(EXIT_SUCCESS);
}
