/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
#include "apue.h"
#include <dirent.h>
#include <limits.h>

/* function type that is called for each filename */
typedef int Func(const char *, const struct stat *, int);

static int locftw(char *, Func *);
/* static Func fn; */
static int fn(const char *pathname, const struct stat *statptr, int type);
static int dopath(Func *);

static long nreg, ndir, nblk, nchar, nfifo, nslink, nsock, ntot;

int main(int argc, char *argv[])
{
	int ret;

	if (argc != 2)
		err_quit("Usage: %s <starting-pathname>", argv[0]);

	ret = locftw(argv[1], fn); /* does it all */

	ntot = nreg + ndir + nblk + nchar + nfifo + nslink + nsock;
	if (ntot == 0)
		ntot = 1;	/* avoid divide by zero; print 0 for all counts */
	printf("regular files\t= %3ld (%5.2f %%)\n", nreg, nreg*100.0/ntot);
	printf("directories\t= %3ld (%5.2f %%)\n", ndir, ndir*100.0/ntot);
	printf("block special\t= %3ld (%5.2f %%)\n", nblk, nblk*100.0/ntot);
	printf("char special\t= %3ld (%5.2f %%)\n", nchar, nchar*100.0/ntot);
	printf("FIFOs\t\t= %3ld (%5.2f %%)\n", nfifo, nfifo*100.0/ntot);
	printf("symbolic links\t= %3ld (%5.2f %%)\n", nslink, nslink*100.0/ntot);
	printf("sockets\t\t= %3ld (%5.2f %%)\n", nsock, nsock*100.0/ntot);
	exit(ret);
}
/*
 * Descend through the hierachy, starting at "pathname".
 * The caller's func() is called for every file.
 */
enum {
	FTW_F,			/* file other than directory */
	FTW_D,			/* directory */
	FTW_DNR,		/* directory that cannot be read */
	FTW_NS,			/* file that we cannot stat */
};

static char *fullpath;		/* contains full pathname for every file */
static size_t pathlen;

static int locftw(char *pathname, Func *func)
{
	fullpath = path_alloc(&pathlen); /* malloc PATH_MAX + 1 */

	if (pathlen <= strlen(pathname)) { /* ??? when can this occur ??? */
		pathlen = strlen(pathname) * 2;
		if ((fullpath = realloc(fullpath, pathlen)) == NULL)
			err_sys("realloc failed");
	}
	strcpy(fullpath, pathname);
	return (dopath(func));
}
/* 
 * Descend through the hierachy, starting at "fullpath".
 * If "fullpath" is anything other than a directory, we lstat() it,
 * recursively for each name in the directory.
 */
static int dopath(Func *func)
{
	struct stat statbuf;
	struct dirent *dirp;
	DIR *dp;
	int ret, n;

	if (lstat(fullpath, &statbuf) < 0) /* stat error */
		return (func(fullpath, &statbuf, FTW_NS));
	if (S_ISDIR(statbuf.st_mode) == 0) /* not a directory */
		return (func(fullpath, &statbuf, FTW_F));

	/*
	 * It's a directory. First call func() for the directory,
	 * then process each filename in the directory.
	 */
	if ((ret = func(fullpath, &statbuf, FTW_D)) != 0)
		return (ret);
	n = strlen(fullpath);
	if (n + NAME_MAX + 2 > pathlen) { /* expand path buffer */
		pathlen *= 2;
		if ((realloc(fullpath, pathlen)) == NULL)
			err_sys("realloc failed");
	}
	fullpath[n++] = '/';
	fullpath[n] = 0;

	if ((dp = opendir(fullpath)) == NULL) /* cannot read directory */
		return(func(fullpath, &statbuf, FTW_DNR));

	while ((dirp = readdir(dp)) != NULL) {
		if (strcmp(dirp->d_name, ".") == 0 ||
		    strcmp(dirp->d_name, "..") == 0)
			continue; /* ignore dot and dot-dot */
		strcpy(&fullpath[n], dirp->d_name); /* append after "/" */
		if ((ret = dopath(func)) != 0)
			break;	/* time to leave */
	}
	fullpath[n-1] = 0;	/* erase everything from slash onward */
	if (closedir(dp) < 0)
		err_ret("Cannot close dir: %s", fullpath);
	return ret;
}

static int fn(const char *pathname, const struct stat *statptr, int type)
{
	switch (type) {
	case FTW_F:
		switch (statptr->st_mode & S_IFMT) {
		case S_IFREG: nreg++; break;
		case S_IFBLK: nblk++; break;
		case S_IFCHR: nchar++; break;
		case S_IFIFO: nfifo++; break;
		case S_IFLNK: nslink++; break;
		case S_IFSOCK: nsock++; break;
		case S_IFDIR: 	/* directories should have type = FTW_D */
			err_dump("for S_IFDIR for %s", pathname);
		}
		break;
	case FTW_D:
		ndir++;
		break;
	case FTW_DNR:
		err_ret("cannot read directory %s", pathname);
		break;
	case FTW_NS:
		err_ret("stat error for %s", pathname);
		break;
	default:
		err_dump("unknown type %d for pathname %s", type, pathname);
	}
	return 0;
}
