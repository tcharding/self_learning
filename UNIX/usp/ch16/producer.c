#include "tch.h"
#include "buffer.h"
#include <fcntl.h>
#include <pthread.h>
#include <sys/stat.h>
#include <dirent.h>

/* static prototypes */
static int is_regular(const char *path);
static int is_dir(const char *path);

/* producer thread function */
void *th_producer(void *args)
{
	/* args: char[2] */
	char **a;
	char *src, *dst;
	struct dirent *dirp;
	DIR *dp;
	buffer_t buft;
	char *srcpath, *dstpath;
	int total;

	total = 0;
	a = (char **)args;
	src = *a;
	dst = *++a;

	if (!is_dir(src))
		err_quit("src directory invalid: %s\n", src);
	if (!is_dir(dst)) {
		if (mkdir(dst, DIR_MODE) == -1)
			err_sys("mkdir error: %s\n", dst);
	}
	if ((dp = opendir(src)) == NULL) {
		err_msg("opendir error: %s\n", src);
		pthread_exit(NULL);
	}
	while ((dirp = readdir(dp)) != NULL) {
		if ((strcmp(dirp->d_name, ".") == 0) ||
		    (strcmp(dirp->d_name, "..") == 0))
			continue; /* ignore dot and dot-dot */
		srcpath = buildpath(src, dirp->d_name);		
		dstpath = buildpath(dst, dirp->d_name);

		if (!is_regular(srcpath))
			continue; /* only regular files implemented */
		total++;
		strncpy(buft.file, dirp->d_name, sizeof(dirp->d_name));
		if ((buft.infd = open(srcpath, O_RDONLY)) == -1) {
			err_msg("in open error: %s\n", srcpath);
			pthread_exit(NULL);
		}

		if ((buft.outfd = open(dstpath, O_WRONLY | O_CREAT | O_TRUNC,
				       FILE_MODE)) == -1) {
			close(buft.infd);
			err_msg("out open error: %s\n", dstpath);
			pthread_exit(NULL);
		}
		free(srcpath);
		free(dstpath);
		putitem(buft);
	}

	setdone();
	pthread_exit((void *)&total);	/* return total files set to copy */
}

/* return TRUE if path is a directory (follows soft links) */
static int is_dir(const char *path)
{
	struct stat sbuf;

	if (stat(path, &sbuf) == -1)
		return FALSE;
	return (S_ISDIR(sbuf.st_mode));
}

/* return TRUE if path is a regular file */
static int is_regular(const char *path)
{
	struct stat sbuf;

	if (stat(path, &sbuf) == -1)
		return FALSE;
	return (S_ISREG(sbuf.st_mode));
}
