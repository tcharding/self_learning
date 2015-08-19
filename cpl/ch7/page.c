#include <stdlib.h>
#include <stdio.h>
#define MAXLINE 1024

static void ncat(FILE *stream);

/* printf file args to stdout with title and line numbers */
int main(int argc, char *argv[])
{
	FILE *fp;
	char *prog, *file;

	prog = argv[0];
	while (--argc > 0) {
		file = *++argv;
		if ((fp = fopen(file, "r")) == NULL) {
			fprintf(stderr, "%s: cannot open %s\n", prog, file);
		} else {
			printf("Title: %s\n", file);
			printf("==================\n");
			ncat(fp);
		}
		puts("");
	}
	exit (0);
}

/* ncat: print stream to stdout */
static void ncat(FILE *stream)
{
	int lineno;
	char readln[MAXLINE];

	lineno = 0;
	while(fgets(readln, MAXLINE, stream) != NULL) {
		++lineno;
		printf("%4d:%s", lineno, readln);
	}
}
