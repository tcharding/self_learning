#include "dirent.h"
#include <stdio.h>
#include <string.h>
#include <fcntl.h>		/* flags for read and write */
#include <sys/types.h>		/* typedefs */
#include <sys/stat.h>		/* structure returned by stat */
#define _POSIX_C_SOURCE 200809L

void fsize(char *);

/* print file size */
int main(int argc, char *argv[])
{
	if (argc == 1) {	/* default: current directory */
		fsize(".");
	} else
		while (--argc > 0)
			fsize(*++argv);
	return 0;
}

/*int stat(char *, struct stat *);*/
void dirwalk(char *, void (*fcn)(char *));

/* fsize: print size of file "name" */
void fsize(char *name)
{
	struct stat stbuf;

	if (stat(name, &stbuf) == -1) {
		fprintf(stderr, "fsize: can't access %s\n", name);
		return;
	}
	/* where are these defined without leading underscores ? */
	if ((stbuf.st_mode & __S_IFMT) == __S_IFDIR) 
		dirwalk(name, fsize);
	printf("%8ld %s\n", stbuf.st_size, name);
}

#define MAX_PATH 1024

/* dirwalk: apply fcn to all files in Dir */
void dirwalk(char *dir, void (*fcn)(char *))
{
	char name[MAX_PATH];
	Dirent *dp;
	DIR *dfd;

	fprintf(stderr, "in dirwalk\n");
	if ((dfd = opendir(dir)) == NULL) {
		fprintf(stderr, "dirwalk: can't open %s\n", dir);
		return;
	}

	while ((dp = readdir(dfd)) != NULL) {
		fprintf(stderr, "dp->name: %s\n", dp->name);
		if (strcmp(dp->name, ".") == 0
		    || strcmp(dp->name, "..") == 0)
			continue; /* skip self and parent */
		if (strlen(dir)+strlen(dp->name)+2 > sizeof(name))
			fprintf(stderr, "dirwalk: name %s/%s too long\n",
				dir, dp->name);
		else {
			sprintf(name, "%s/%s", dir, dp->name);
			fprintf(stderr, "fcn(name): %s\n", name);
			(*fcn)(name);
		}
	}
}

