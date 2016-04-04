/* Exercise 12.3 */
#include <pwd.h>
#include <dirent.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static Boolean isAbsolute(const char *path);
static Boolean isPidDir(const char *name);
static void doPidDir(const char *psName, const char *queryPath);

static ssize_t buildPsPath(char *buf, const char *name);
static char *fdPathFromPsName(char *fdPath, const char *psName);

/* list all processes that have path (argv[1]) open */
int
main(int argc, char *argv[])
{
	char *path;
	DIR * dirp;
	struct dirent *dp;

	if (argc < 2 || strcmp(argv[1], "--help") == 0) {
		usageErr("%s /absoute/path\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	
	path = argv[1];
	if (!isAbsolute(path)) {
		fprintf(stderr, "path does not appear absolute: %s\n", path);
		exit(EXIT_FAILURE);
	}

	dirp = opendir("/proc");
	if (dirp == NULL)
		errExit("Error opening %s", path);

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;

		if (isPidDir(dp->d_name))
			doPidDir(dp->d_name, path);
	}

}


static Boolean
isAbsolute(const char *path)
{
	if (path == NULL)
		return FALSE;

	return (path[0] == '/');
}

/* isPidDir: true if dir name is all digits */
static Boolean
isPidDir(const char *name)
{
	char path[PATH_MAX];
	struct stat sb;
	
	if (buildPsPath(path, name) == -1)
		return FALSE;
				/* check is dir (and present) */
	if (stat(path, &sb) == -1)
		return FALSE;
	if (!S_ISDIR(sb.st_mode))
		return FALSE;
				/* check is all digits */
	for ( ;*name != '\0'; ++name)
		if (!isdigit(*name))
			return FALSE;
	return TRUE;
}

/* psDoDir: process a /proc/PID directory */
static void
doPidDir(const char *psName, const char *queryPath)
{
	char fdPath[PATH_MAX];
	char symLink[PATH_MAX];
	DIR * dirp;
	struct dirent *dp;
	struct stat sb;
	ssize_t len;

	if (fdPathFromPsName(fdPath, psName) == NULL)
		errExit("fdPathFromPsName");

	dirp = opendir(fdPath);
	if (dirp == NULL)
		return;		/* don't have read permissions */

	if (chdir(fdPath) == -1)
		return;

	for (;;) {
		errno = 0;
		dp = readdir(dirp);

		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;

		if (lstat(dp->d_name, &sb) == -1)
			errExit("stat");
		if (S_ISLNK(sb.st_mode)) {
			len = readlink(dp->d_name, symLink, PATH_MAX);
			if (len == -1)
				errExit("readlink");

			if (strstr(symLink, queryPath) != NULL)
				fprintf(stderr, "%s\n", psName);
		} 
	}
}


/* psBuildPath: builds path /proc/PID in buf using name */
static ssize_t
buildPsPath(char *buf, const char *name)
{
	char *proc = "/proc/";
	ssize_t len;
	
	if (name == NULL || *name == '\0')
		return -1;

	len = strlen(proc) + strlen(name) + 1;
	if (len > PATH_MAX)
		return -1;
	
	strcpy(buf, proc);
	strcpy(buf+strlen(proc), name);
	buf[len-1] = '\0';

	return len-1;
}

/* fdPathFromPsName: put into fdPath '/proc/psName/fd' */
static char *
fdPathFromPsName(char *fdPath, const char *psName)
{
	char *proc = "/proc/";
	char *fd = "/fd/";
	
	strcpy(fdPath, proc);
	strcpy(fdPath+strlen(proc), psName);
	strcpy(fdPath+strlen(proc)+strlen(psName), fd);

	return fdPath;
}
