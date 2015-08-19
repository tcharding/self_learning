#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define MAXLINE 1024

enum {
	EXCEPT = 0x2,
	NUMBER = 0x4,
	NAME = 0x8
};
int find(FILE *stream, char *s, int flags, char *file);

/* minimal grep, no regex */
int main(int argc, char *argv[])
{
        char *prog, *infile;
	char *sterm;		/* search term */
	int c, found;
	FILE *fp;
	int flags = 0;

	prog = argv[0];
	if (argc < 2) {
		fprintf(stderr,
			"Usage: %s [-n -x -H] <search> [file1 file2 ...]\n", prog);
		exit(1);
	}
	/* process options */
	while (--argc > 0 && (*++argv)[0] == '-') 
		while ((c = *++argv[0]))
			switch (c) {
			case 'x':
				flags |= EXCEPT;
				break;
			case 'n':
				flags |= NUMBER;
				break;
			case 'H':
				flags |= NAME;
				break;
			default:
				fprintf(stderr, "%s: illegal option %c\n", prog, c);
				argc = 0;
				found = -1;
				break;
			}
	/* get search term */
	sterm = *argv++;
	--argc;
	
	if (argc == 0){		/* stdin */
		found = find(stdin, sterm, flags, NULL);
	} else {		/* arg files */
		if (argc > 1)
			flags |= NUMBER;
		while (argc-- > 0) {
			infile = *argv++;
			if ((fp = fopen(infile, "r")) == NULL) {
				fprintf(stderr,
					"%s: cannot open %s\n", prog, infile);
				exit(2);
			}
			found = find(fp, sterm, flags, infile);
		}
	}
	return found;
}


/* find: fin */
int find(FILE *stream, char *s, int flags, char *file)
{
	char readln[MAXLINE];
	int lineno, found, except;

	except = (flags & EXCEPT) ? 1 : 0;
	lineno = found = 0;
	while (fgets(readln, MAXLINE, stream) != NULL) {
		lineno++;
		if ((strstr(readln, s) != NULL) != except) {
			if (flags & NAME)
				printf("%s:", file);

			if (flags & NUMBER)
				printf("%d:", lineno);
			printf("%s", readln);
			found++;
		} 
	}
	return found;
}
