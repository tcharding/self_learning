#include "tch.h"
#include <fcntl.h>
#include <dirent.h>
#include <pthread.h>

#define OFLAGS (O_WRONLY | O_TRUNC | O_CREAT)
#define OPERMS (S_IRUSR | S_IWUSR)

static void *cpdir(void *arg);
static void *copyfilepass(void *arg);
static int cpfile(int infd, int outfd);
static char *buildpath(const char *dir, const char *file);


int main(int argc, char *argv[])
{
	FILE *stream;
	size_t size;
	char *buf;
	pthread_t tid;
	
	if (argc != 3)
		err_quit("Usage: %s src_dir dst_dir", argv[0]);
	if ((stream = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	fprintf(stream, "%s%s", argv[1], argv[2]);
	fclose(stream);
	Pthread_create(&tid, NULL, cpdir, (void *)buf);
	pthread_join(tid, NULL);
	free(buf);
	return 0;
}

/* cpdir: copy all regular files in src to dst,
    arg is of form 'src\0dst' */
static void *cpdir(void *arg)
{
	char *src_dir, *dst_dir;
	char *src_path, *dst_path;
	struct dirent *dirp;
	DIR *dp;
	int fd[3];		/* infd, outfd, retval */
	pthread_t tid;

				/* get inputs */
	src_dir = (char *)arg;
	dst_dir = (char *)arg;
	while (*dst_dir++ != '\0')
		;	

	
	if ((dp = opendir(src_dir)) == NULL)
		return NULL;
	while ((dirp = readdir(dp)) != NULL) {
		src_path = buildpath(src_dir, dirp->d_name);
		if ((fd[0] = open(src_path, O_RDONLY)) < 0) 
		    err_sys("failed to open %s", src_path);
		dst_path = buildpath(dst_dir, dirp->d_name);
		if ((fd[1] = open(dst_path, OFLAGS, OPERMS)) < 0)
		    err_sys("failed to open %s", dst_path);
		fd[2] = 0;
		Pthread_create(&tid, NULL, copyfilepass, fd);
		pthread_join(tid, NULL);
		free(src_path);
		free(dst_path);
	}
	if (closedir(dp) < 0)
		err_msg("failed to close directory: %s", src_path);
	return (void *)0;
}

static void *copyfilepass(void *arg)
{
	int *argint;

	argint = (int *)arg;
	argint[2] = cpfile(argint[0], argint[1]);
	Close(argint[0]);
	Close(argint[1]);
	return (void *)(argint + 2);
}

/* buildpath: return pathname dir/file, must be free'd */
static char *buildpath(const char *dir, const char *file)
{
	char *path;
	
	path = path_alloc(NULL);

	strncpy(path, dir, strlen(dir));
	if (*(path + strlen(path) - 1) != '/')
		strcat(path, "/");
	strcat(path, file);
	return path;
}

/* copyfile: copy infd to outfd */
static int cpfile(int infd, int outfd)
{
	char buf[BUFSIZ];
	int read;
	int total = 0;

	for ( ; ; ) {
		while ((read = Readn(infd, buf, BUFSIZ)) > 0) {
			Writen(outfd, buf, read);
			total += read;
		}
	}
	return total;
}
