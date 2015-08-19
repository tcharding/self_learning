/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
#include "apue.h"
#include <fcntl.h>
#define OFFSET 16384

char buf1[] = "abcdefghij";
char buf2[] = "ABCDEFGHIJ";

/* create file with a hole in it */
int main(int argc, char *argv[])
{
	int fd;

	if ((fd = creat("file.hole", FILE_MODE)) < 0)
		err_sys("creat error");

	if (write(fd, buf1, 10) != 10)
		err_sys("buf1 write error");
	/* offset now 10 */

	if (lseek(fd, OFFSET, SEEK_SET) == -1)
		err_sys("lseek error");
	/* offset now OFFSET */

	if (write(fd, buf2, 10) != 10)
		err_sys("buf2 write error");
	/* offset now OFFSET + 10 */
	return 0;
}
	
