/* Exercise 14.1 */
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <dirent.h>
#include "tlpi_hdr.h"

static Boolean isAccessable(const char *path);
static int manyFiles(int num, const char *path);
static int unlinkManyFiles(const char *path);

static char *randFileName(char *buf);

int main(int argc, char *argv[])
{
	int num;
	int created, unlinked;
	char *path;

	if (argc < 3 || strcmp(argv[1], "--help") == 0) 
		usageErr("%s num dir", argv[0]);

	num = getInt(argv[1], GN_ANY_BASE, "num");
	path = argv[2];

	if (!isAccessable(path)) {
		eprintf("path is not writable: %s\n", path);
		exit(EXIT_FAILURE);
	}

	if ((created = manyFiles(num, path)) != num)
		eprintf("Error, created %d of %d files.\n", created, num);

	if ((unlinked = unlinkManyFiles(path)) != created)
		eprintf("Error, unlinked %d of %d files.\n", unlinked, created);
	
	exit(EXIT_SUCCESS);
}

#define DIR_MODE R_OK | W_OK | X_OK;

/* isAccessable: true if path is writable */
static Boolean
isAccessable(const char *path)
{
	int dirMode = DIR_MODE;
	
	if (access(path, dirMode))
		return FALSE;

	return TRUE;
}

#define FLAGS O_CREAT | O_WRONLY
#define LEN 8			/* x + 6*N + '\0' */

/* manyFiles: create num random files in directory path */
static int
manyFiles(int num, const char *path)
{
	int i;
	int fd;
	int created;
	char buf[LEN];

	created = 0;
	srand((int) time(NULL));
	
	if (chdir(path) == -1) {
		eprintf("failed to chdir: %s\n", path);
		return 1;
	}
	
	for (i = 0; i < num; i++) {
		randFileName(buf);
		if ((fd = open(buf, FLAGS, S_IRWXU)) == -1)
			errMsg("failed to create: %s\n", buf);
		else
			created++;
		if (write(fd, "x", 1) == -1)
			errMsg("failed to write byte to file: %s\n", buf);
		
		if (close(fd))
			eprintf("failed to close: %s\n", buf);
	}
	return created;
}

/* unlinkManyFiles: unlink files created by manyFiles() */
static int
unlinkManyFiles(const char *path)
{
	DIR * dirp;
	struct dirent *dp;
	int unlinked;

	unlinked = 0;

	if (chdir(path) == -1) {
		eprintf("failed to chdir: %s\n", path);
		return 1;
	}
	
	dirp = opendir(path);
	if (dirp == NULL)
		errExit("opendir: %s", path);

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;

		if (dp->d_name[0] =='x') {
			if (unlink(dp->d_name) == -1)
				eprintf("Failed to unlink: %s\n", dp->d_name);
			else
				unlinked++;
		}
	}
	return unlinked;
}

#define ASCII_ZERO 48

/* randFileName: create random file name form: xNNNNNN */
static char *
randFileName(char *buf)
{
	int i;
	
	buf[0] = 'x';

	for (i = 1; i < LEN-1; i++) {
		buf[i] = ASCII_ZERO + rand() % 10;
	}
	buf[LEN-1] = '\0';

	return buf;
}
