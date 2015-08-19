#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#define MAXLINE 1024

void diff(FILE *f, FILE *g);

/* compare two files */
int main(int argc, char *argv[])
{
	char *prog, *inf1, *inf2;
	FILE *fp1, *fp2;

	prog = argv[0];
	if (argc != 3) {
		fprintf(stderr, "Usage: %s <file1> <file2>\n", prog);
		exit(1);
	} 

	inf1 = argv[1];
	inf2 = argv[2];
	if ((fp1 = fopen(inf1, "r")) == NULL) {
		fprintf(stderr, "%s: cannot open %s\n", prog, inf1);
		exit(2);
	}
	if ((fp2 = fopen(inf2, "r")) == NULL) {
		fprintf(stderr, "%s: cannot open %s\n", prog, inf2);
		exit(2);
	}
	diff(fp1, fp2);
	exit(0);
}

/* diff: compare two streams */
void diff(FILE *f, FILE *g)
{
	char r[MAXLINE], s[MAXLINE];
	int rres, sres;
	int cnt;

	cnt = 0;
	for ( ; ; ) {
		++cnt;
		rres = (fgets(r, MAXLINE, f) == NULL) ? 0 : 1;
		sres = (fgets(s, MAXLINE, g) == NULL) ? 0 : 1;
		if (rres == 0 && sres == 0)
			return;		/* we are done */
		if (rres == 0)
			printf("%d: > %s", cnt, s);
		else if (sres == 0)
			printf("%d: < %s",cnt, r);
		else if (strcmp(r, s) != 0) {
			printf("%d: < %s",cnt, r);
			printf("%d: > %s", cnt, s);
		}
	}
}
