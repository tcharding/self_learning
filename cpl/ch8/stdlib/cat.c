#include "stdio.h"
#include <unistd.h>

FILE _iob[OPEN_MAX] = {		/* stdin, stout, stderr */
	{ 0, (char *) 0, (char *) 0, _READ, 0 },
	{ 0, (char *) 0, (char *) 0, _WRITE, 1 },
	{ 0, (char *) 0, (char *) 0, _WRITE | _UNBUF, 2 },
};

void cat(FILE *fp);

int main(int argc, char *argv[])
{
	FILE *fp;
	
	if (argc == 1) {	
		cat(stdin);
	} else {		/* concatenate arguments */
		while (--argc > 0) {
			if ((fp = fopen(*++argv, "r")) == NULL)
				continue;
			cat(fp);
			fclose(fp);
		}
	}
	return 0;
}

void cat(FILE *fp)
{
	int c;
	
	while ((c = getc(fp)) != EOF) {
		putc(c, stdout);
		if (c == '\n')
			fflush(stdout);
	}
}
