#include "unp.h"
#include "readline.h"

int main(int argc, char **argv)
{
	int count = 0;
	ssize_t	n;
	char recvline[MAXLINE];
	struct readln rl;
	
	readline_rinit(STDIN_FILENO, recvline, MAXLINE, &rl);

	while ( ( n = readline_r(&rl)) > 0) {
		count++;
	}
	printf("%d lines\n", count);
	exit (0);
}
