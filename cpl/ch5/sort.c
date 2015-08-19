#include "sort.h"
#include <ctype.h>

/* static prototypes */
static void qsort(void *lineptr[], int left, int right,
		  int (*comp)(void *, void *));
static void swap(void *v[], int i, int j);
static int numcmp(const char *, const char *);
static int ignstrcmp(const char *, const char *);
static int dordcmp(const char *, const char *);

/* sort input lines */
int main(int argc, char *argv[])
{
	char *lineptr[MAXLINES];
	int c;
	int nlines;		/* number of input lines read */
	int (*comp)(const char *, const char *);
	int numeric = 0;	/* 1 if numeric sort */
	int ign = 0;		/* 1 if ignore case */
	int reverse = 0;	/* 1 if reverse order */
	int dorder = 0;		/* 1 if directory order */
	
	while (--argc > 0 && **++argv == '-') {
		while ( (c = *++argv[0])) {
			switch (c) {
			case 'd':
				dorder = 1;
				break;
			case 'f':
				ign = 1;
				break;
			case 'n':
				numeric = 1;
				break;
			case 'r':
				reverse = 1;
				break;
			default:
				printf("sort: illegal option %c\n", c);
				argc = 0;
				break;
			}
		}
	}
	if (numeric)
		comp = numcmp;
	else if (ign)
		comp = ignstrcmp;
	else if (dorder)
		comp = dordcmp;
	else
		comp = strcmp;
	
	if ( (nlines = readlines(lineptr, MAXLINES)) >= 0) {
		qsort((void **)lineptr, 0, nlines-1, (int (*)(void*,void*)) comp);
		writelines(lineptr, nlines, reverse);
		return 0;
	} else {
		printf("input too big to sort\n");
		return 1;
	}
}

/* qsort: sort v[left]...v[right] into increasing order */
static void qsort(void *v[], int left, int right, int (*comp)(void *, void *))
{
	int i, last;
	
	if (left >= right)	/* do nothing if array contains one elem */
		return;
	swap(v, left, (left + right) / 2); /* move partition elem to v[0] */
	last = left;
	for (i = left+1; i <= right; i++) /* partition */
		if ((*comp)(v[i], v[left]) < 0)
			swap(v, ++last, i);
	swap(v, left, last);	/* restore partition elem */
	qsort(v, left, last-1, comp);
	qsort(v, last+1, right, comp);
}

/* ignstrcmp: compare s1 and s2 ignoring case */
static int ignstrcmp(const char *s1, const char *s2)
{
	char b1[BUFSIZE], b2[BUFSIZE];
	int i;
	
	strncpy(b1, s1, BUFSIZE);
	strncpy(b2, s2, BUFSIZE);

	for (i = 0; b1[i] != '\0'; i++)
		b1[i] = tolower(b1[i]);

	for (i = 0; b2[i] != '\0'; i++)
		b2[i] = tolower(b2[i]);

	return strcmp(b1, b2);
}


/* numcmp: compare s1 and s2 numerically */
static int numcmp(const char *s1, const char *s2)
{
	double v1 = atof(s1);
	double v2 = atof(s2);

	if (v1 < v2)
		return -1;
	else
	return (v1 > v2) ? 1 : 0; 

	/*
	else if (v1 > v2)
		return 1;
	else
		return 0; */
}

/* dordcmp: compare s1 and s2 ignoring leading '.' */
static int dordcmp(const char *s1, const char *s2)
{
	if (*s1 == '.')
		s1++;
	if (*s2 == '.')
		s2++;
	return strcmp(s1, s2);
}
/* swap: interchange v[i] and v[j] */
static void swap(void *v[], int i, int j)
{
	void *temp;

	temp = v[i];
	v[i] = v[j];
	v[j] = temp;
}
