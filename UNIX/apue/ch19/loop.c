#include "apue.h"

int loop(int ptym, int ignoreeof)
{
	int n, maxfd, nread;
	fd_set rset;
	char buf[BUFSIZ];

	FD_ZERO(&rset);
	maxfd = max(ptym, STDIN_FILENO);
	
	for ( ; ; ) {
		FD_SET(ptym, &rset);
		FD_SET(STDIN_FILENO, &rset);
		if ((n = select(maxfd+1, &rset, NULL, NULL, NULL)) < 0)
			err_sys("select error");

		if (FD_ISSET(STDIN_FILENO, &rset)) {
			if ((nread = read(STDIN_FILENO, buf, BUFSIZ)) < 0)
				err_sys("read error from stdin");
			else if (nread == 0)
				break;
			if (writen(ptym, buf, nread) != nread)
				err_sys("writen error to master pty");
		}
		if (FD_ISSET(ptym, &rset)) {
			if ((nread = read(ptym, buf, BUFSIZ)) <= 0)
				break;
			if (writen(STDOUT_FILENO, buf, nread) != nread)
				err_sys("writen error to stdout");
		}
	}
	return 0;
}
