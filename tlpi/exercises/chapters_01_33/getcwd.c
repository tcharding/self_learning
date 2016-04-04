/* Exercise  18.5 */
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include "tlpi_hdr.h"

static char *myGetcwd(char *buf);

static char *getBasenameCwd(char *buf);
static void appendFileToPath(const char *file, char *path);
static void prependFileToPath(const char *file, char *path);
static char *strcpyR(char *dst, const char *src);

static ino_t getIno(const char *path);

/* tests */
static void test(void);
static void testGetBasenameCwd(void);
static void testStrcpyR(void);
static void testAppendFileToPath(void);

int
main(int argc, char *argv[]) {
	char cwd[PATH_MAX];

	test();
	bzero(cwd, sizeof(cwd));

	if (argc > 1) {
		if (chdir(argv[1]) == -1)
			errExit("chdir");
	}
	
	if (myGetcwd(cwd) == NULL)
		errExit("getcwd");

	puts(cwd);
	
	exit(EXIT_SUCCESS);
}

/* getcwd: put current working directory into buf */
static char *
myGetcwd(char *buf)
{
	char basename[PATH_MAX];

	bzero(basename, sizeof(basename));
	
	for (;;) {
		if (getBasenameCwd(basename) == NULL)
			errExit("getBasenameCwd");

		prependFileToPath(basename, buf);
		if (strcmp(basename, "/") == 0)
			break;
		if (chdir("..") == -1)
			errExit("chdir: ..");
	}

	return buf;
}

/* getBasenameCwd: return basename in buf */
static char *
getBasenameCwd(char *buf)
{
	struct dirent *dp;
	DIR *dirp;
	ino_t inoD, inoP;

	inoD = getIno(".");
	inoP = getIno("..");

	if (inoD == -1 || inoP == -1) {
		fprintf(stderr, "failed to get inode numbers");
		return NULL;
	}

	if (inoD == inoP) {
		strcpy(buf, "/");
		return buf;
	} 
		
	dirp = opendir("..");
	if (dirp == NULL)
		errExit("opendir: ..");

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;

		if (dp->d_ino == inoD) {
			strcpy(buf, dp->d_name);
			return buf;
		}
	}
	return NULL;
}

/* appendFileToPath: append file and slashes as joining slash */
static void
appendFileToPath(const char *file, char *path)
{
	if (path[strlen(path)-1] != '/')
		strcat(path, "/");
	strcat(path, file);
}

/* prependFileToPath: add file to front of path conserving slashes */
static void
prependFileToPath(const char *file, char *path)
{
	char buf[PATH_MAX];

	strcpy(buf, path);
	strcpy(path, "/");
	strcpy(path, file);
/* if (file[strlen(file)-1] == '/')
		file[strlen(file)-1] == '\0';
*/
	strcpy(path, buf);
}
/* strcpyR: reverse and copy string */
static char *
strcpyR(char *dst, const char *src)
{
	size_t len;
	int i, j;

	len = strlen(src);

	for (i = len-1, j = 0; i >= 0; i--, j++) {
		dst[j] = src[i];
	}
	
	dst[len] = '\0';
	return dst;
}

/* getIno: get inode numeber */
static ino_t getIno(const char *path)
{
	struct dirent *dp;
	DIR *dirp;

	dirp = opendir(path);
	if (dirp == NULL)
		return (ino_t)-1;

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0)
			return dp->d_ino;
	}

	return (ino_t)-1;
}

static void
test(void)
{
	testGetBasenameCwd();
	testAppendFileToPath();
	testStrcpyR();
}

static void
testGetBasenameCwd(void)
{
	char cwd[PATH_MAX];
	char basename[PATH_MAX];

	if (getcwd(cwd, PATH_MAX) == NULL)
		errExit("getcwd");
	chdir("/home/tobin");
	if (getBasenameCwd(basename) == NULL)
		errExit("getBasenameCwd");
	if (strcmp(basename, "tobin"))
		errExit("Error: getBasenameCwd: %s\n", basename);
}

static void
testAppendFileToPath(void)
{
	char path[PATH_MAX];

	strcpy(path, "/");
	appendFileToPath("test", path);
	if (strcmp(path, "/test"))
		errExit("test append file to path: %s", path);

	appendFileToPath("test", path);
	if (strcmp(path, "/test/test"))
		errExit("test test append file to path");
	
}

static void
testStrcpyR(void)
{
	char dst[PATH_MAX];
	char *backward, *forward;

	backward = "edcba";
	forward = "abcde";
	strcpyR(dst, backward);
	if (strcmp(dst, forward))
		eprintf("got: %s exp: %s\n", dst, forward);
}
