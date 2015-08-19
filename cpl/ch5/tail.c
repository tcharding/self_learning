#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
	N = 10,			/* default number of lines to print */
	BUFSIZE = 4096
};

int readlines(char *lineptr[], int nlines);
void writelines(char *lineptr[], int nlines);
int getline(char *s, int lim);

/* write last n lines */
int main(int argc, char *argv[])
{
	int n, i;
	int nlines;
	char *lineptr[BUFSIZE], *ptr;
	
	if (argc > 1) {
		ptr = *++argv;
		if (strcmp(ptr, "-h") == 0 || strcmp(ptr, "--help") == 0) {
			printf("Usage: %s [nlines]\n", *--argv);
			return 0;
		} else {
			n = (*ptr == '-') ? atoi(++ptr) : atoi(ptr);
		}
	} else 
		n = N;
	
	if ( (nlines = readlines(lineptr, BUFSIZE)) > 0)
		for (i = 0; i < n; i++) {
			printf("%s\n", lineptr[nlines-1 - i]);
	} else
		fprintf(stderr, "Error: readlines failed\n");
	return 0;
}

/* readlines: read up to nlines into lineptr */
int readlines(char *lineptr[], int maxlines)
{
	int len, nlines;
	char *p, line[BUFSIZE];

	nlines = 0;
	while ( (len = getline(line, BUFSIZE)) > 0) {
		if (len == EOF || nlines >= maxlines)
			break;
		if ( (p = malloc(len)) == NULL) { /* no need +1 */
			return -1;
		}
		line[len-1] = '\0'; /* delete newline */
		strcpy(p, line);
		lineptr[nlines++] = p;
	}
	return nlines;
}

/* writelines: write output lines */
void writelines(char *lineptr[], int nlines)
{
	while (nlines-- > 0)
		printf("%s\n", *lineptr++);
}


/* getline: read a line into s, strip trailing whitespace, return length */
int getline(char *s, int lim)
{
	int c, i;

	for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; i++, s++)
		*s = c;

	if (c == EOF && i == 0)
		return EOF;

	if (c == '\n') {
		*s++ = c;
		i++;
	}
	*s = '\0';
	return i;
}
