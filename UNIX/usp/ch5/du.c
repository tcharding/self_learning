#include "tch.h"
#include "stack.h"
#include <dirent.h>

/* TODO
 * add options to main and flags to depthfirstapply
 */

/* function to apply to each file */
typedef int Func(const char *);

int depthfirstapply(const char *path, Func *);
int breadthfirstapply(const char *path, Func *);
/* static int echo_name(const char *path); */
static int sizepathfun(const char *path);

int main(int argc, char *argv[])
{
	int total = 0;
	char *path;
	
	if (argc != 2)
		err_quit("Usage: %s path", argv[0]);
	path = argv[1];

	if (chdir(path) != 0)	/* cannot execute path */
		err_sys("failed to chdir(%s)\n", path);
	total = breadthfirstapply(path, sizepathfun);
	printf("\n%d\ttotal\n", total);

	/* st_free(stp); */
	return 0;
}


/* depthfirstapply: apply fn recursively to path. 
   Does not follow symbolic links */
int depthfirstapply(const char *path, Func *func)
{
	int total, res;
	struct stat statbuf;
	struct dirent *dirp;
	DIR *dp;
	char *file;
	struct stack *stp;
	
	if ((dp = opendir(".")) == NULL) /* cannot read path */
		return -1;

	total = 0;
	stp = st_creat();
	bzero(&statbuf, sizeof(statbuf));
	while ((dirp = readdir(dp)) != NULL) {
		if (strcmp(dirp->d_name, ".") == 0 ||
		    strcmp(dirp->d_name, "..") == 0)
			continue; /* ignore dot and dot-dot */
		if (*dirp->d_name == '.' )
			continue; /* ignore hidden files */
		if (stat(dirp->d_name, &statbuf) < 0)
			continue; /* if we cannot stat it, skip it */
		if (S_ISREG(statbuf.st_mode)) {
			st_push(stp, dirp->d_name);
			continue;
		} else if (S_ISDIR(statbuf.st_mode)) {
			if (chdir(dirp->d_name) != 0)	/* cannot execute path */
				return -1;
			if ((res = depthfirstapply(dirp->d_name, func)) == -1)
				return -1;
			else
				total += res;
			if (chdir("..") != 0)	/* cannot execute path */
				err_sys("failed to chdir(..)");
		}
				/* implicitly skip all other file types */
	}
	while ((file = st_pop(stp)) != NULL) { /* apply to stack */
		if ((res = func(file)) != -1)
			total += res;
		free(file); /* free memory popped from stack */
	}
	if ((res = func(file)) != -1)
		total += res;
	st_free(stp);
	return total;
}

/* breadthfirstapply: apply fn recursively to path. 
   Does not follow symbolic links */
int breadthfirstapply(const char *path, Func *func)
{
	int total, res;
	struct stat statbuf;
	struct dirent *dirp;
	DIR *dp;
	char *file;
	struct stack *stp;
	
	if ((dp = opendir(".")) == NULL) /* cannot read path */
		return -1;

	total = 0;
	stp = st_creat();
	bzero(&statbuf, sizeof(statbuf));
	if ((res = func(path)) != -1)
		total += res;
	while ((dirp = readdir(dp)) != NULL) {
		if (strcmp(dirp->d_name, ".") == 0 ||
		    strcmp(dirp->d_name, "..") == 0)
			continue; /* ignore dot and dot-dot */
		if (*dirp->d_name == '.' )
			continue; /* ignore hidden files */
		if (stat(dirp->d_name, &statbuf) < 0)
			continue; /* if we cannot stat it, skip it */
		if (S_ISREG(statbuf.st_mode)) {
			total += func(dirp->d_name);
		} else if (S_ISDIR(statbuf.st_mode)) {
			st_push(stp, dirp->d_name);
			continue;
		}
				/* implicitly skip all other file types */
	}
	while ((file = st_pop(stp)) != NULL) { /* apply to stack */
		if (chdir(file) != 0)	/* cannot execute path */
				return -1;
		if ((res = breadthfirstapply(file, func)) == -1)
			return -1;
		else
			total += res;
		if (chdir("..") != 0)	/* cannot execute path */
			err_sys("failed to chdir(..)");
		free(file); /* free memory popped from stack */
	}
	st_free(stp);
	return total;
}


/* /\* echo_name: write file to stdout *\/ */
/* static int echo_name(const char *file) */
/* { */
/* 	if (file != NULL) { */
/* 		fprintf(stdout, "%s\n", file); */
/* 		return 1;		/\* there is one file *\/ */
/* 	} */
/* 	return 0; */
/* } */


/*
 * BUG: information returned by stat is all zero
 */
/* sizepathfun: write size of path in blocks and path to stdout */
static int sizepathfun(const char *path)
{
	struct stat statbuf;

	bzero(&statbuf, sizeof(statbuf));
	if (stat(path, &statbuf) > 0)
		return -1;

	printf("st_size:%ld\tst_blocks:%ld\tst_blksize:%ld\t%s\n",
	       statbuf.st_size, statbuf.st_blocks, statbuf.st_blksize, path);
	return statbuf.st_size;
}
