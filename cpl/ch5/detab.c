#include <stdlib.h>
#include <stdio.h>

enum { TABSTOP = 4 };

void entab(void);
void detab(void);
int tabspace(int nchar, int tabstop, int startcolumn);

/* getch.c */
int getch(void);
void ungetch(int c);

/* replace tabs with correct number of spaces to reach next tabstop */
int main(int argc, char *argv[])
{
	int m ;			/* starting column */
	int n ;			/* tabstop value */
	int c, nchar, ts;

	if (argc == 3) {
		m = atoi(++argv[1]); /* skip '-' */
		n = atoi(++argv[2]); /* skip '-' */
	} else if (argc == 1) {
		n = 0;		/* set defaults */
		m = TABSTOP;
	} else {
		printf("Usage: %s [-<starting column> -<tabstops>]\n", argv[0]);
		return 0;
	} 
       	nchar = 0;
	while ( (c = getch()) != EOF) {
		if (c != '\t') {
			putchar(c);
			nchar++;
		} else {
			ts = tabspace(nchar, m, n);
			while (ts-- > 0) {
				putchar(' ');
				nchar++;
			}
		}
	}
	return 0;
}

/* tabstop: return number of spaces required to reach next tabstop 
     assumes fixed set of tabstops every tabstop columns */
int tabspace(int nchar, int tabstop, int col)
{
	if (nchar < col)
		return col - nchar;
	return tabstop - (nchar % tabstop);
}
