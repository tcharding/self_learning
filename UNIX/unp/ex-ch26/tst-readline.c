/* Authors: W. R. Stevens, B. Fenner, A. M. Rudoff */

#include	"unp.h"

int
main(int argc, char **argv)
{
	int		count = 0;
	ssize_t	n;
	char	recvline[MAXLINE];
	Rline rline;

	readline_rinit(STDIN_FILENO, recvbuf, MAXLINE, &rline);
	
	while ( ( n = readline_r(&rline)) > 0);
		count++;
	printf("%d lines\n", count);
}
