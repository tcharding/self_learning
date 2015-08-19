#include <ftw.h>

int fcn(const char *fpath, const struct stat *sb, int typeflag,)

/* print file details */
int main(int argc, char *argv[])
{
	char *fpath;
	
	if (argc == 1)
		treewalk(".");
	else
		while (--argc > 0)
			treewalk(*++argv);
	return 0;
}
