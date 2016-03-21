/* Exercise 12.1 */
#include <pwd.h>
#include <dirent.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static void ps(const char *user);

static Boolean isPidDir(const char *name);
static void psDoDir(const char *user, const char *name);

static ssize_t buildPsPath(char *buf, const char *name);
static FILE *statusFileFromPsName(const char *name);
static Boolean lineStartsWith(const char *line, const char *startsWith);
static Boolean userMatchesLine(const char *user, const char *uidLine);
static void printCommand(const char *nameLine);

static uid_t getUidFromLine(const char *uidLine);

int
main(int argc, char *argv[])
{
	char *user;
	struct passwd *pwd;
	
	if (argc == 1)
		usageErr("%s user", argv[0]);
	user = argv[1];

	errno = 0;
	pwd = getpwnam(user);
	if (pwd == NULL) {
		if (errno != 0)
			errExit("getpwnam");
		else {
			fprintf(stderr, "user does not appear to exist: %s\n", user);
			exit(EXIT_FAILURE);
		}
	}
	
	ps(user);

	exit(EXIT_SUCCESS);
}

/* ps: list all processes owned by user */
static void
ps(const char *user)
{
	DIR * dirp;
	struct dirent *dp;
	
	dirp = opendir("/proc");
	if (dirp == NULL)
		errExit("Error opening /proc");

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;
		/* TODO: check if dp is file or directory */
		if (isPidDir(dp->d_name))
			psDoDir(user, dp->d_name);
	}
}


/* isPidDir: true if dir name is all digits */
static Boolean
isPidDir(const char *name)
{
	char path[PATH_MAX];
	struct stat sb;
	
	if (buildPsPath(path, name) == -1)
		return FALSE;
				/* check is dir */
	if (stat(path, &sb) == -1)
		return FALSE;
	
	if ((sb.st_mode & S_IFMT) != S_IFDIR)
		return FALSE;
				/* check is all digits */
	for ( ;*name != '\0'; ++name)
		if (!isdigit(*name))
			return FALSE;
	return TRUE;
}

/* psDoDir: process a /proc/PID directory */
static void
psDoDir(const char *user, const char *psName)
{
	FILE *status;
	char line[255];
	char *uidLine, *nameLine;
	
	status = statusFileFromPsName(psName);
	if (status == NULL)
		return;		/* process may have exited */
	
	while (fgets(line, sizeof(line), status)) {
		if (lineStartsWith(line, "Uid:")) {
			uidLine = alloca(strlen(line+1));
			if (uidLine == NULL)
				errExit("alloca");
			strcpy(uidLine, line);
		}
			
		if (lineStartsWith(line, "Name:")) {
			nameLine = alloca(strlen(line+1));
			if (nameLine == NULL)
				errExit("alloca");
			strcpy(nameLine, line);
		}
	}

	if (userMatchesLine(user, uidLine)) {
		printf("%s ", psName);
		printCommand(nameLine);
	}
}

/* open stream for file /pid/<name>/status */
static FILE *
statusFileFromPsName(const char *name)
{
	char buf[PATH_MAX];
	ssize_t len;

	len = buildPsPath(buf, name); /* /proc/PID */
	if (len == -1)
		return NULL;

	strcpy(buf+len, "/status");
	return fopen(buf, "r");
}

/* lineStartsWith: true if first characters of line are startsWith */
static Boolean
lineStartsWith(const char *line, const char *startsWith)
{
	const char *ptr;

	for (ptr = startsWith; *ptr != '\0'; ptr++, line++) {
		if (*ptr != *line)
			return FALSE;
	}
	return TRUE;
}

/* userMatchesLine: true if uid from uidLine matches user uid */
static Boolean
userMatchesLine(const char *user, const char *uidLine)
{
	struct passwd *pwd;
	uid_t luid;

	pwd = getpwnam(user);
	if (pwd == NULL) {
		printf("user does not appear to exist: %s\n", user);
		return FALSE;
	}
	luid = getUidFromLine(uidLine);

	return (pwd->pw_uid == luid) ? TRUE : FALSE;
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

static void
printCommand(const char *nameLine)
{
	char *token;
	char *cpy;

	if (nameLine == NULL || *nameLine == '\0')
		errExit("can't call with null");

	cpy = alloca(strlen(nameLine)+1);
	strcpy(cpy, nameLine);

	token = strtok (cpy, "\t");
	if (strcmp(token, "Name:") != 0)
		errExit("First token does not appear to be Name:");

	token = strtok (NULL, " ");
	printf("%s", token);
}

static uid_t
getUidFromLine(const char *uidLine)
{
	char *token;
	uid_t uid;
	char *cpy;

	if (uidLine == NULL || *uidLine == '\0')
		errExit("can't call with null");

	cpy = alloca(strlen(uidLine)+1);
	strcpy(cpy, uidLine);

	token = strtok (cpy, "\t");
	if (strcmp(token, "Uid:") != 0)
		errExit("First token does not appear to be Uid: %s (%s)", uidLine, token);

	token = strtok (NULL, " ");
	uid = (uid_t) strtol(token, NULL, 10);

	return uid;
}
