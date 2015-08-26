#include "tch.h"

int processchar(int fdin, int fdout, char c, const char *s);

/* test processchar() */
int main(void)
{
	fprintf(stderr, "Transforming 'c' into 'string'\n");
	if (processchar(STDIN_FILENO, STDOUT_FILENO, 'c', "string") == -1)
		fprintf(stderr, "processchar failed\n");

	fprintf(stderr, "processchar success\n");
	return 0;
}
