#ifndef _SORT_H
#define _SORT_H
#include <stdio.h>
#include <string.h>

enum {
	BUFSIZE = 4096,
	MAXLINES= 4096
};

/* prototypes in atof.c */
double atof(const char s[]);

/* prototypes in readlines.c */
int readlines(char *lineptr[], int nlines);
void writelines(char *lineptr[], int nlines, int rflag);

#endif	/* _SORT_H */
