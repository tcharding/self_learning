#include "apue.h"
#include <sys/stat.h>
#include <sys/types.h>
/* determine socket stat members */
int main(void)
{
	struct stat sbuf;
	int sockfd;

	bzero(&sbuf, sizeof(struct stat));
	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		err_sys("socket error");
	if (fstat(sockfd, &sbuf) == -1)
		err_sys("fstat error");
	printf("TCP socket stat buf\n");
	printf("dev: %d ino: %d mode: %d nlink: %d uid: %d gid: %d rdev: %d\n",
	       (int)sbuf.st_dev, (int)sbuf.st_ino, (int)sbuf.st_mode,
	       (int)sbuf.st_nlink,(int)sbuf.st_uid, (int)sbuf.st_gid,
	       (int)sbuf.st_rdev);
	exit(0);
}
