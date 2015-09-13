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

static copyinfo_t *ci_init(const char *name, pthread_t tid);
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
	int infd, outfd;
	pthread_t tid;
	struct stat statbuf;
	copyinfo_t *cip, *newci;

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
	if (stat(dst_dir, &statbuf) == -1) {
		if (mkdir(dst_dir, DIR_MODE) == -1)
			err_sys("mkdir error");
	}

	while ((dirp = readdir(dp)) != NULL) {
		if ((!strcmp(dirp->d_name, ".")) ||
		    (!strcmp(dirp->d_name, "..")))
			continue;
		src_path = buildpath(src_dir, dirp->d_name);
		fprintf(stderr, "src_path: %s\n", src_path);
		if ((infd = open(src_path, O_RDONLY)) < 0) 
		    err_sys("failed to open %s", src_path);
		dst_path = buildpath(dst_dir, dirp->d_name);
		fprintf(stderr, "dst_path: %s\n", dst_path);
		if ((outfd = open(dst_path, OFLAGS, FILE_MODE)) < 0)
		    err_sys("failed to open %s", dst_path);
	
		newci = ci_init(dst_path, tid);
		newci->sourcefd = infd;
		newci->destinationfd = outfd;
		newci->next = head;
		head = newci;
		Pthread_create(&newci->tid, NULL, copyfilepass, (void *)newci);
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
static copyinfo_t *ci_init(const char *name, pthread_t tid)
{
	copyinfo_t *cip;

	cip = Malloc(sizeof(copyinfo_t));
	bzero(cip, sizeof(cip));
	cip->namestring = s_dup(name);
	cip->tid = tid;
	cip->next = NULL;
	return cip;
}

/* usp Program 12.7 */
static void *copyfilepass(void *arg)
{
	copyinfo_t *cip;

	cip = (copyinfo_t *)arg;
	cip->bytescopied = copyfile(cip->sourcefd, cip->destinationfd);
	r_close(cip->sourcefd);
	r_close(cip->destinationfd);
	return (void *)(&cip->bytescopied);
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

