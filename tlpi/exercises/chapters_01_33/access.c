/* Exercise 15.4 */
#include <fcntl.h>
#include <sys/stat.h>
#include "tlpi_hdr.h"

enum {
	F_IOK = 0x01,
	R_IOK = 0x02,
	W_IOK = 0x04,
	X_IOK = 0x08,
};

static int myAccess(const char *file, int flags);


/* test myAccess() */
int
main(void)
{
	char *file = "/tmp/tfile";
	int fd;
	
	unlink(file);

	fd = open(file, O_WRONLY | O_CREAT, 0644);

	if (myAccess(file, F_IOK) != 0)
		errExit("myAccess: false negative F_IOK");
	if (myAccess(file, R_IOK) != 0)
		errExit("myAccess: false negative R_IOK");
	if (myAccess(file, W_IOK) != 0)
		errExit("myAccess: false negative W_IOK");

	if (myAccess(file, X_IOK) == 0)
		errExit("myAccess: false positive X_IOK");

	fchmod(fd, 0044);
	
	if (myAccess(file, F_IOK) != 0)
		errExit("myAccess: false negative F_IOK");
	if (myAccess(file, R_IOK) == 0)
		errExit("myAccess: false positive R_IOK");
	if (myAccess(file, W_IOK) == 0)
		errExit("myAccess: false positive W_IOK");

	exit(EXIT_SUCCESS);
}

/* myAccess: check effective users myAccess rights */
static int
myAccess(const char *file, int flags)
{
	struct stat sb;

	if (stat(file, &sb) == -1)
		return -1;

	if (flags & R_IOK) 
		if (!(sb.st_mode & S_IRUSR))
			return -1;

	if (flags & W_IOK) 
		if (!(sb.st_mode & S_IWUSR))
			return -1;

	if (flags & X_IOK) 
		if (!(sb.st_mode & S_IXUSR))
			return -1;

	return 0;
}
       
