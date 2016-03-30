/* Exercise 27.2 */
#include <stdarg.h>
#include <sys/stat.h>
#include <limits.h>
#include <dirent.h>
#include <ctype.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

static int ourExeclp(const char *pat, ...);
static Boolean isAbsolute(const char *path);
static char *getPath(const char *file, char *buf);
static char *combineDirFileInBuf(const char *dir, const char *file, char buf[]);
static Boolean dirHasFile(const char *dir, const char *file);

/* dynamic vector of strings */
struct dv {
	char **v;
	int size;
	int count;
};

struct dv *dvInit(void);
void dvDestroy(struct dv *dvp);
int dvAdd(struct dv *dvp, char *s);
void dvDump(struct dv *dvp);

/* testing */
static void runTests(void);

int
main(int argc, char *argv[]) {
	char *script = "dummy_out.sh";

	runTests();
	
	if (argc > 1 && strcmp(argv[1], "--help") == 0)
		usageErr("%s [TEST]\n", argv[0]);

	setbuf(stdout, NULL);

	ourExeclp(script, "arg0", "arg1", "arg2", NULL );
	errExit("execlp");
}

/* ourExeclp: implementation of execlp(3) using execve(2) */
static int
ourExeclp(const char *file, ...)
{
	extern char **environ;
	va_list ap;
	char *arg;
	struct dv *dv;
	char *dupstr;
	char buf[PATH_MAX];
	
	dv = dvInit();
	if (dv == NULL)
		errExit("dvInit");
	
	va_start(ap, file);
	for (;;) {
		arg = va_arg(ap, char *);
		if (arg == NULL)
			break;
		dupstr = strdup(arg);
		if (dupstr == NULL)
			errExit("dupstr");
		if (dvAdd(dv, dupstr) == -1)
			errExit("dvAdd");
	}
	va_end(ap);

	if (isAbsolute(file))
		strcpy(buf, file);
	else
		if (getPath(file, buf) == NULL)
			return -1;

	execve(buf, dv->v, environ);

	return -1;		/* shouldn't get here */
}

/* isAbsolute: true if path is absolute */
static Boolean
isAbsolute(const char *path)
{
	if (path == NULL)
		return FALSE;

	return (path[0] == '/');
}

#define DEFAULT_PATH "/bin:/usr/bin"

/* getPath: search PATH for first occurance of file, 
   if found put absolute path in buf, return pointer to buf else return NULL */
static char *
getPath(const char *file, char *buf)
{
	char *penv, *dupstr;
	char *tok;

	penv = dupstr = tok = NULL;
	
	penv = getenv("PATH");
	if (penv == NULL)
		penv = DEFAULT_PATH;

	dupstr = strdup(penv);
	if (dupstr == NULL)
		errExit("strdup");

	for (tok = strtok(dupstr, ":"); tok != NULL; tok = strtok(NULL, ":")) {
		if (dirHasFile(tok, file)) {
			return combineDirFileInBuf(tok, file, buf);
		}		
	} 

	return NULL;
}

/* dirHasFile: true if dir contains file */
static Boolean
dirHasFile(const char *dir, const char *file)
{
	DIR * dirp;
	struct dirent *dp;
	
	dirp = opendir(dir);
	if (dirp == NULL)
		return FALSE;

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;
		if (strcmp(dp->d_name, file) == 0)
			return TRUE;
	}
	return FALSE;
}

/* combineDirFileInBuf: build path out of dir and file */
static char *
combineDirFileInBuf(const char *dir, const char *file, char buf[])
{
	
	strcpy(buf, dir);
	if (buf[strlen(buf)-1] != '/')
		strcat(buf, "/");

	strcat(buf, file);

	return buf;
}


#define INIT_SIZE 1

/* dvInit: initialise dynamic vector */
struct dv *
dvInit(void)
{
	struct dv *dvp;

	dvp = malloc(sizeof(struct dv));
	if (dvp == NULL)
		return NULL;

	dvp->v = calloc(INIT_SIZE+1, sizeof(char *));
	if (dvp->v == NULL)
		return NULL;
	*(dvp->v) = NULL;
	dvp->size = INIT_SIZE;
	dvp->count = 0;
	
	return dvp;
}

/* dvDestroy: destroy dvp, calls free on all strings */
void
dvDestroy(struct dv *dvp)
{
	char **v;
	
	if (dvp != NULL) {
		if (dvp->v != NULL) {
			for (v = dvp->v; *v != NULL; v++)
				free(*v);
			free(dvp->v);
		}
		free(dvp);
	}
}

/* dvAdd: add s to dvp, return current count or -1 on error */
int
dvAdd(struct dv *dvp, char *s)
{
	char **v;
	int numSlots;

	if (dvp->count >= dvp->size) {	      /* increase dvp->v */
		numSlots = dvp->size * 2 + 1; /* +1 for NULL */
		v = realloc(dvp->v, numSlots * sizeof(char *));
		if (v == NULL)
			return -1; /* realloc sets errno */
		dvp->v = v;
		dvp->size = numSlots - 1;
	}

	for (v = dvp->v; *v != NULL; v++)
		;		/* seek to end */

	*v = s;
	v++;
	*v = NULL;
	dvp->count++;

	return dvp->count;
}

/* dvDump: dump dvp->v to stderr */
void dvDump(struct dv *dvp)
{
	char **v;

	fprintf(stderr, "Vector Dump:\n");
	for (v = dvp->v; *v != NULL; v++)
		fprintf(stderr, "%s\n", *v);
}


static void
testGetPath(void)
{
	char *ps = "/usr/bin/ps";
	char *d = "dummy_out.sh";
	char *dpath = "/home/tobin/bin/dummy_out.sh";
	
	char buf[PATH_MAX];

	if (getPath("ps", buf) == NULL)
		errExit("test: getPath");

	if (strcmp(ps, buf) != 0)
		fprintf(stderr, "Fail: exp: '%s' got: '%s'\n", ps, buf);

	if (getPath(d, buf) == NULL)
		errExit("test: getPath");

	if (strcmp(dpath, buf) != 0) {
		fprintf(stderr, "Fail: exp: '%s' got: '%s'\n", dpath, buf);
		exit(1);
	}
}

static void
testCombineDirFileInBuf()
{
	char buf[PATH_MAX];
	char *dir;
	char *file;
	char *exp;
	
	dir = "/home";
	file = "tfile";
	combineDirFileInBuf(dir, file, buf);

	exp = "/home/tfile";
	if (strcmp(buf, exp) != 0) {
		printf("FAIL: exp: %s got: %s\n", exp, buf);
		exit(1);
	}
		
}

static void
runTests(void)
{
	testCombineDirFileInBuf();
	testGetPath();

}
