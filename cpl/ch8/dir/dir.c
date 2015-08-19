#include "dirent.h"
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef DIRSIZ
#define DIRSIZ 14
#endif
struct direct {			/* directory entry */
	ino_t d_ino;		/* inode number */
	char d_name[DIRSIZ];	/* long name does not have '\0' */
};

/* System dependent functions; Version 7 and System V UNIX */

int fstat(int fd, struct stat *);

/* opendir: open a directory for readdir calls */
DIR *opendir(char *dirname)
{
	int fd;
	struct stat stbuf;
	DIR *dp;

	if ((fd = open(dirname, O_RDONLY, 0)) == -1
	    || fstat(fd, &stbuf) == -1
	    || (stbuf.st_mode & __S_IFMT) != __S_IFDIR
	    || (dp = (DIR *) malloc(sizeof(DIR))) == NULL)
		return NULL;
	dp->fd = fd;
	return dp;
}

/* closedir: close directory opened by opendir */
void closedir(DIR *dp)
{
	if (dp) {
		close(dp->fd);
		free(dp);
	}
}

/* readdir: read directory entries in sequence */
Dirent *readdir(DIR *dp)
{
	struct direct dirbuf;	/* local directory structure */
	static Dirent d;	/* return: portable structure */

	while (read(dp->fd,  (char *) &dirbuf, sizeof(dirbuf)) == sizeof(dirbuf)) {
		if (dirbuf.d_ino == 0) /* slot not in use */
			continue;
		d.ino = dirbuf.d_ino;
		strncpy(d.name, dirbuf.d_name, DIRSIZ);
		d.name[DIRSIZ] = '\0'; /* ensure termination */
		return &d;
	}
	return NULL;
}
