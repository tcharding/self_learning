/* Exercise 18.3 */
#include <limits.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static char *myRealpath(const char *path, char *realpath);

static void removeDir(char *path);
static void appendTokenToPath(const char *file, char *path);
static Boolean isDir(const char *path);

static void appendFileToPath(const char *file, char *path);

/* tests */
static void test(void);

static void testMyRealpath(void);
static void testAppendFileToPath(void);
static void testRemoveDir(void);

static void checkMyRealpath(const char *path, const char *expected);


/* test realpath */
int
main(int argc, char *argv[]) {

	test();

	exit(EXIT_SUCCESS);
}

/* myRealpath: resolve path into realpath */
static char *
myRealpath(const char *path, char *realpath)
{
	char pathCopy[PATH_MAX];
	char *token;

	if (strlen(path) > PATH_MAX)
		errExit("path too long");
	strcpy(pathCopy, path);

	if (pathCopy[0] == '/') {
		strcpy(realpath, "/");
		if (chdir("/") == -1)
			errExit("chdir: /");
	} else {
		realpath[0] = '\0';
	}
	
	token = strtok(pathCopy, "/");
	
	while (token != NULL) {
		if (strcmp(token, ".") == 0) {
			;
		} else if (strcmp(token, "..") == 0) {
			removeDir(realpath);
			if (chdir("..") == -1)
				errExit("chdir: ..");
		} else {
			appendTokenToPath(token, realpath);
			if (isDir(token))
				if (chdir(token) == -1)
					errExit("chdir: token %s", token);
		}

		token = strtok(NULL, "/");
	}

	return realpath;
}

static void
removeDir(char *path)
{
	char *slash;

	if (strlen(path) == 1)
		return;

	slash = strrchr(path, '/');
	if (*(slash+1) == '\0') {
		*slash = '\0';
		slash = strrchr(path, '/');
	}
	*slash = '\0';
	if (strlen(path) == 0)
		strcpy(path, "/");
}

/* appendTokenToPath: conditionally checks if token is symlink */
static void
appendTokenToPath(const char *file, char *path)
{
	struct stat sb;
	char buf[PATH_MAX];

	bzero(buf, sizeof(buf));

	if (lstat(file, &sb) == -1)
		errExit("lstat");

	if (S_ISLNK(sb.st_mode)) {
		if (readlink(file, buf, PATH_MAX) == -1)
			errExit("readlink: %s", file);
		strcpy(path, buf);
	} else {
		appendFileToPath(file, path);
	}
}

static Boolean
isDir(const char *path)
{
	struct stat sb;

	if (stat(path, &sb) == -1)
		return FALSE;
	return (S_ISDIR(sb.st_mode));
}

/*l appendFileToPath: append file and slashes as joining slash */
static void
appendFileToPath(const char *file, char *path)
{
	if (path[strlen(path)-1] != '/')
		strcat(path, "/");
	strcat(path, file);
}

static void
test(void)
{
	testAppendFileToPath();
	testRemoveDir(); 

	testMyRealpath();
}

static void
testMyRealpath(void)
{

	/* initial check */
	checkMyRealpath("/", "/");

	/* dot, dot dot, and multiple slashes */
	checkMyRealpath("/home//tobin", "/home/tobin");

	checkMyRealpath("/home/tobin/scratch/../scratch/cover.txt",
		 "/home/tobin/scratch/cover.txt");

	checkMyRealpath("/home/tobin///", "/home/tobin");

	checkMyRealpath("/home/tobin/.", "/home/tobin");

	checkMyRealpath("/home/tobin/../tobin/././../tobin", "/home/tobin");

	/* soft links */
	checkMyRealpath("/home/tobin/scratch/err.c",
		   "/home/tobin/build/c/err.c"); /* file */
	
	checkMyRealpath("/home/tobin/scratch/build", "/home/tobin/build"); /* dir */

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
testRemoveDir(void)
{
	char path[PATH_MAX];

	strcpy(path, "/");
	removeDir(path);
	if (strcmp(path, "/"))
		errExit("removeDir 1");

	appendFileToPath("test", path);
	removeDir(path);
	if (strcmp(path, "/"))
		errExit("removeDir 2");

	appendFileToPath("test", path);
	strcat(path, "/");
	removeDir(path);
	if (strcmp(path, "/"))
		errExit("removeDir 3: %s", path);
	
	strcpy(path, "/home/tobin/scratch");
	removeDir(path);
	if (strcmp(path, "/home/tobin"))
		errExit("removeDir 4");

}

static void
checkMyRealpath(const char *path, const char *expected)
{
	static int counter = 0;
	char realpath[PATH_MAX];

	bzero(realpath, sizeof(realpath));
	
	myRealpath(path, realpath);
	if (strcmp(expected, realpath) != 0) 
		eprintf("Fail (%d): exp: %s got: %s\n", counter, expected, realpath);

	counter++;
}
