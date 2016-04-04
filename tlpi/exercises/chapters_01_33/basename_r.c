/* Exercise 31.2 see dirname_r also */
#include <libgen.h>
#include <pthread.h>
#include <limits.h>
#include "tlpi_hdr.h"

/* use thread specific data */

static pthread_once_t once = PTHREAD_ONCE_INIT;
static pthread_key_t key;

char *basename_r(const char *path);

static void
destructor(void *buf)
{
	;			/* TODO */
}

static void
createKey(void)
{
	int err;

	err = pthread_key_create(&key, destructor);
	if (err != 0)
		errExitEN(err, "key_create");
}

char *basename_r(const char *path)
{
	char *buf;
	int err;
	char localPath[PATH_MAX];

	strcpy(localPath, path);

	err = pthread_once(&once, createKey);
	if (err != 0)
		errExitEN(err, "once");

	buf = pthread_getspecific(key);
	if (buf == NULL) {	/* first time */
		buf = malloc(PATH_MAX);
		if (buf == NULL)
			errExit("malloc");

		err = pthread_setspecific(key, buf);
		if (err != 0)
			errExitEN(err, "setspecific");
	}

	strcpy(buf, basename(localPath));

	return buf;
}

/* test implementation */
int
main(int argc, char *argv[]) {
	char *basename;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s path\n", argv[0]);

	basename = basename_r(argv[1]);
	printf("%s\n", basename);
	
	exit(EXIT_SUCCESS);
}
