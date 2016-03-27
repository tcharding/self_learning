/* Exercise 18.8 */
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include "tlpi_hdr.h"
#include "stack.h"

typedef int Func(const char *, struct stat *);

static Func fn;
static int ftw(const char *dirpath, Func *fn, int flags);
static void readDirToStacks(struct stack *dirStack, struct stack *regStack);


#define FTW_DEPTH 1

/* test ftw */
int
main(int argc, char *argv[]) {

	int opt;
	char *path;
	int typeflag;

	typeflag = 0;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s [-d] path\n", argv[0]);

	while (( opt = getopt(argc, argv, "d")) != -1) {
		switch(opt) {
		case 'd':
			typeflag |= FTW_DEPTH;
			break;
		default:
			errExit("unknow option: %s", argv[optind]);
		}
	}
	if (optind >= argc)
		errExit("expected pathname");
	path = argv[optind];

	
	if (ftw(path, fn, typeflag) == -1)
		errExit("nftw");

	exit(EXIT_SUCCESS);
}

/* ftw: file tree walk */
static int
ftw(const char *dirpath, int (*fn) (const char *, struct stat *), int flags) 
{
	struct stack *regStack, *dirStack;
	char path[PATH_MAX];
	
	regStack = newStack();
	dirStack = newStack();

	if (chdir(dirpath) == -1)
		errExit("chdir: %s", dirpath);
	
	readDirToStacks(dirStack, regStack);

	if (flags & FTW_DEPTH) {
		while (pop(dirStack, path, PATH_MAX) != NULL) 
			ftw(path, fn, flags);
		while (pop(regStack, path, PATH_MAX) != NULL) 
			ftw(path, fn, flags);
	} else {		/* breadth first */
		while (pop(regStack, path, PATH_MAX) != NULL) 
			ftw(path, fn, flags);
		while (pop(dirStack, path, PATH_MAX) != NULL) 
			ftw(path, fn, flags);
	}

	if (chdir("..") == -1)
		errExit("chdir ..");
	return 0;
}

/* readDirToStacks: read cwd adding files/dir respectively to stacks */
static void
readDirToStacks(struct stack *dirStack, struct stack *regStack)
{
	struct stat sb;
	DIR *dirp;
	struct dirent *dp;

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;

		if (lstat(dp->d_name, &sb) == -1) {
			fprintf(stderr, "failed to stat: %s\n", dp->d_name);
			continue;
		}
		if (S_ISDIR(sb.st_mode))
			push(dirStack, dp->d_name);

		else if (S_ISREG(sb.st_mode))
			push(regStack, dp->d_name);
	}
}

/* fn: simple test function for ftw */
static int
fn(const char *fpath, struct stat *sb)
{
	printf("%s\n", fpath);
	
	return 0;
}
