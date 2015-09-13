#include "tch.h"
#include "restart.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <pthread.h>
#define BLKSIZE 1024
#define OFLAGS (O_WRONLY | O_TRUNC | O_CREAT)

typedef struct copy_struct {
	char *namestring;
	int sourcefd;
	int destinationfd;
	int bytescopied;
	pthread_t tid;
	struct copy_struct *next;
} copyinfo_t;
copyinfo_t *head = NULL;
copyinfo_t *tail = NULL;

static copyinfo_t *ci_init(const char *name, int fd[], pthread_t tid);
static void *copydirectory(void *arg);
static void *copyfilepass(void *arg);
static char *buildpath(const char *dir, const char *file);


int main(int argc, char *argv[])
{
	FILE *stream;
	size_t size;
	char *buf;
	pthread_t tid;

	if (argc != 3)
		err_quit("Usage: %s src_dir dst_dir", argv[0]);
	buf = NULL;
	tid = 0;
	if ((stream = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	fprintf(stream, "%s", argv[1]);
	fputc('\0', stream);
	fprintf(stream, "%s", argv[2]);
	if (fclose(stream) > 0)
		err_sys("fclose error");

	Pthread_create(&tid, NULL, copydirectory, (void *)buf);
	(void)pthread_join(tid, NULL);
	free(buf);
	return 0;
}

/* copydirectory: copy all regular files in src to dst,
    arg is of form 'src\0dst\0' */
static void *copydirectory(void *arg)
{
	char *src_dir, *dst_dir;
	char *src_path, *dst_path;
	struct dirent *dirp;
	DIR *dp;
	int fd[3];		/* infd, outfd, retval */
	pthread_t tid;
	struct stat statbuf;
	copyinfo_t *cip;

				/* get inputs */
	src_dir = (char *)arg;
	dst_dir = (char *)arg;
	while (*dst_dir++ != '\0')
		;	
	fprintf(stderr, "copydirectory got: %s %s\n", src_dir, dst_dir);
	tid = 0;
	if ((dp = opendir(src_dir)) == NULL) {
		err_quit("Failed to opendir: %s", src_dir);
	}
	if (stat(dst_dir

		 , &statbuf) == -1) {
		if (mkdir(dst_dir, DIR_MODE) == -1)
			err_sys("mkdir error");
	}

	while ((dirp = readdir(dp)) != NULL) {
		if ((!strcmp(dirp->d_name, ".")) ||
		    (!strcmp(dirp->d_name, "..")))
			continue;
		src_path = buildpath(src_dir, dirp->d_name);
		fprintf(stderr, "src_path: %s\n", src_path);
		if ((fd[0] = open(src_path, O_RDONLY)) < 0) 
		    err_sys("failed to open %s", src_path);
		dst_path = buildpath(dst_dir, dirp->d_name);
		fprintf(stderr, "dst_path: %s\n", dst_path);
			

		if ((fd[1] = open(dst_path, OFLAGS, FILE_MODE)) < 0)
		    err_sys("failed to open %s", dst_path);
		fd[2] = 0;
		Pthread_create(&tid, NULL, copyfilepass, fd);
		cip = ci_init(dst_path, fd, tid);
		cip->next = head;
		head = cip;
		for (cip = head; cip != NULL; cip = cip->next) {
			(void)pthread_join(cip->tid, NULL);			
		}

		free(src_path);
		free(dst_path);
	}
	if (closedir(dp) < 0)
		err_msg("failed to close directory");
	return (void *)0;
}

/* ci_init: allocate memory for new copyinfo ci */
static copyinfo_t *ci_init(const char *name, int fd[], pthread_t tid)
{
	copyinfo_t *cip;

	cip = Malloc(sizeof(copyinfo_t));
	bzero(cip, sizeof(cip));
	cip->namestring = s_dup(name);
	cip->sourcefd = fd[0];
	cip->destinationfd = fd[1];
	cip->bytescopied = fd[2];
	cip->tid = tid;
	cip->next = NULL;
	return cip;
}

/* usp Program 12.7 */
static void *copyfilepass(void *arg)
{
	int *argint;

	argint = (int *)arg;
	argint[2] = copyfile(argint[0], argint[1]);
	r_close(argint[0]);
	r_close(argint[1]);
	return (void *)(argint + 2);
}

/* buildpath: return pathname dir/file, must be free'd */
static char *buildpath(const char *dir, const char *file)
{
	FILE *stream;
	size_t size;
	char *buf;

	buf = NULL;

	if ((stream = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	fprintf(stream, "%s", dir);
	if (*(dir + strlen(dir) - 1) != '/')
		fputs("/", stream);
	fprintf(stream, "%s", file);
	if (fclose(stream) > 0)
		err_sys("fclose error");
	return buf;
}

