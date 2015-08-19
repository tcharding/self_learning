#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
	MAXLINES = 4096,
	BUFSIZE = 2048,
};



int readlines(char *lineptr[], int nlines);
void writelines(char *lineptr[], int nlines);
int th_getline(char *s, int lim);
void th_qsort(char *lineptr[], int left, int right);
static void swap(char *v[], int i, int j);

/* sort input lines */
int main(void)
{
	char *lineptr[MAXLINES]; /* pointers to text lines */
	int nlines;		 /* number of input lines read */
	
	if ( (nlines = readlines(lineptr, MAXLINES)) >= 0) {
		th_qsort(lineptr, 0, nlines-1);
		writelines(lineptr, nlines);
		return 0;
	} else {
		printf("error: too big to sort\n");
		return 1;
	}
}

/* readlines: read up to nlines into lineptr */
int readlines(char *lineptr[], int maxlines)
{
	int len, nlines;
	char *p, line[BUFSIZE];

	nlines = 0;
	while ( (len = th_getline(line, BUFSIZE)) > 0) {
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

/* th_qsort: sort v[left]...v[right] into increasing order */
void th_qsort(char *v[], int left, int right)
{
	int i, last;

	if (left >= right)	/* do nothing if array contains one elem */
		return;
	swap(v, left, (left + right) / 2); /* move partition elem to v[0] */
	last = left;
	for (i = left+1; i <= right; i++) /* partition */
		if (strcmp(v[i], v[left]) < 0)
			swap(v, ++last, i);
	swap(v, left, last);	/* restore partition elem */
	th_qsort(v, left, last-1);
	th_qsort(v, last+1, right);
}


/* th_getline: read a line into s, strip trailing whitespace, return length */
int th_getline(char *s, int lim)
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

/* swap: interchange v[i] and v[j] */
static void swap(char *v[], int i, int j)
{
	char *temp;

	temp = v[i];
	v[i] = v[j];
	v[j] = temp;
}
