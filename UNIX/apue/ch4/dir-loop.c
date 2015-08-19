#include "apue.h"

/* test directory tree depth system limits */
int main(void)
{
	int i;
	char *name = "tall_tree";

	i = 0;
	if (chdir("/tmp") == -1)
		err_sys("chdir fail: cannot chdir to home");
	for ( ; ; ) {
		++i;
		fprintf(stderr, "%d\n", i);
		if (mkdir(name, DIR_MODE) == -1)
			err_sys("failed to make dir#:%d", i);
		if (chdir(name) == -1)
			err_sys("failed to chdir int dir#:%d", i);
	}
	return 0;
}
