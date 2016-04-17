/* Exercise 44.1 */
#include <sys/wait.h>
#include <ctype.h>
#include "tlpi_hdr.h"

/* use two pipes for bi-directional communication */

static void upperCase(char *buf, int size);

int
main(int argc, char *argv[]) {
	int pfdDown[2];
	int pfdUp[2];
	char buf[BUF_SIZE];
	int nread;

	if (pipe(pfdDown) == -1 ||
	    pipe(pfdUp) == -1)
		errExit("pipe");
			
	switch(fork()) {
	case -1:
		errExit("fork");
		break;
	case 0:			/* child */
		if (close(pfdDown[W]) == -1 ||
		    close(pfdUp[R]) == -1)
			errExit("close");
		while ((nread = read(pfdDown[R], buf, sizeof(buf))) > 0) {
			upperCase(buf, nread);
			if (write(pfdUp[W], buf, nread) != nread)
				errExit("child: failed/partial write to pipe");
		}
		if (nread == -1)
			errExit("child: pipe read error");

		_exit(EXIT_SUCCESS);
	default:
		break;		/* parent falls through */
	}

	if (close(pfdDown[R]) == -1 ||
	    close(pfdUp[W]) == -1)
		errExit("close");
	while ((nread = read(STDIN_FILENO, buf, sizeof(buf))) > 0) {
		if (write(pfdDown[W], buf, nread) != nread)
			errExit("parent: failed/partial write to pipe");
		nread = read(pfdUp[R], buf, sizeof(buf));
		if (write(STDOUT_FILENO, buf, nread) != nread)
			errExit("parent: failed/partial write to stdout");
	}
	if (nread == -1)
		errExit("parent: read error");
	wait(NULL);
	exit(EXIT_SUCCESS);

}

/* upperCase: convert buf to all uppercase */
static void
upperCase(char *buf, int size)
{
	int i;

	for (i = 0; i < size; i++)
		buf[i] = toupper(buf[i]);
}
