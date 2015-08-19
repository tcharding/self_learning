#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
	BUFSIZE = 4096
};

/* prototypes of functions difined in this file */
int readlines(char *lineptr[], int nlines);
void writelines(char *lineptr[], int nlines, int rflag);
int getline(char *s, int lim);


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
void writelines(char *lineptr[], int nlines, int reverse)
{

	if (reverse) {
		while (nlines-- > 0)
			printf("%s\n", lineptr[nlines]);
	} else {
		while (nlines-- > 0)
			printf("%s\n", *lineptr++);
	}

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
