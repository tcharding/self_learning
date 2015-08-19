#include "apue.h"
#include <dirent.h>
#include <limits.h>

/* function type that is called for each filename */
typedef int Func(const char *, const struct stat *, int);

static int locftw(char *rootdir, Func *);
/* static Func fn; */
static int fn(const char *pathname, const struct stat *statptr, int type);
static int dopath(char *filename, Func *);

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
	printf("regular files\t= %7ld (%5.2f %%)\n", nreg, nreg*100.0/ntot);
	printf("directories\t= %7ld (%5.2f %%)\n", ndir, ndir*100.0/ntot);
	printf("block special\t= %7ld (%5.2f %%)\n", nblk, nblk*100.0/ntot);
	printf("char special\t= %7ld (%5.2f %%)\n", nchar, nchar*100.0/ntot);
	printf("FIFOs\t\t= %7ld (%5.2f %%)\n", nfifo, nfifo*100.0/ntot);
	printf("symbolic links\t= %7ld (%5.2f %%)\n", nslink, nslink*100.0/ntot);
	printf("sockets\t\t= %7ld (%5.2f %%)\n", nsock, nsock*100.0/ntot);
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
	FTW_NX,			/* directory without execute permission */
};

static int locftw(char *rootdir, Func *func)
{

	if (chdir(rootdir) == -1)
		err_sys("root dir is not accessable: %s", rootdir);
	return (dopath(rootdir, func));
}
/* 
 * Descend through the hierachy, starting at "fullpath".
 * If "fullpath" is anything other than a directory, we lstat() it,
 * recursively for each name in the directory.
 */
static int dopath(char *filename, Func *func)
{
	struct stat statbuf;
	struct dirent *dirp;
	DIR *dp;
	int ret;

	if (lstat(filename, &statbuf) < 0) { /* stat error */
		fprintf(stderr, "lsat filename: %s\n", filename);
		return (func(filename, &statbuf, FTW_NS));
	}
	if (S_ISDIR(statbuf.st_mode) == 0) /* not a directory */
		return (func(filename, &statbuf, FTW_F));

				/* It's a directory. */
	if (chdir(filename) == -1) /* fails if execute bit is not set */
		return (func(filename, &statbuf, FTW_NX));
	if ((ret = func(filename, &statbuf, FTW_D)) != 0) /* generalise dopath() */
		return (ret);

	if ((dp = opendir(".")) == NULL) { /* cannot read directory */
		if (chdir("..") == -1)
			err_sys("failed to change up directory");
		return(func(filename, &statbuf, FTW_DNR));
	}
	while ((dirp = readdir(dp)) != NULL) {
		if (strcmp(dirp->d_name, ".") == 0 ||
		    strcmp(dirp->d_name, "..") == 0)
			continue; /* ignore dot and dot-dot */
		/* strcpy(&fullpath[n], dirp->d_name); /\* append after "/" *\/ */
		if ((ret = dopath(dirp->d_name, func)) != 0)
			break;	/* time to leave */
	}
	/* fullpath[n-1] = 0;	/\* erase everything from slash onward *\/ */
	if (closedir(dp) < 0)
		err_ret("Cannot close dir: %s", filename);
	if (chdir("..") == -1)
		err_sys("failed to change up directory");
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
	case FTW_NX:
		err_ret("cannot enter the directory %s", pathname);
		break;
	default:
		err_dump("unknown type %d for pathname %s", type, pathname);
	}
	return 0;
}
