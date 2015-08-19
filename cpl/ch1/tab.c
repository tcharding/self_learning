#include <stdio.h>

enum { TABSTOP = 4 };

void entab(void);
void detab(void);
int tabspace(int nchar, int tabstop);

/* getch.c */
int getch(void);
void ungetch(int c);

int main(int argc, char *argv[])
{
	entab();
	/*detab();*/

	return 0;
}

/* detab: replace tabs with correct number of spaces to reach next tabstop */
void detab(void)
{
	int c, nchar, n;

	nchar = 0;
	while ( (c = getch()) != EOF) {
		if (c != '\t') {
			putchar(c);
			nchar++;
		} else {
			n = tabspace(nchar, TABSTOP);
			while (n-- > 0) {
				putchar(' ');
				nchar++;
			}
		}
	}
}
		     
/* entab: replace blanks with minimum number of tabs and spaces */
void entab(void)
{
	int c, nchars, cnt;
	int nspaces, ntabs;

	nchars = 0;
	while ( (c = getch()) != EOF) {
		++nchars;
		if ( c == ' ') {
			cnt = 1;
			while ((c = getch()) == ' ' && c != EOF) 
				cnt++; /* count spaces */

			ungetch(c); /* read one char too many */
			ntabs = cnt / TABSTOP; 
			nspaces = cnt % TABSTOP;
			while (ntabs-- > 0)
				putchar('\t');
			while (nspaces-- > 0)
				putchar(' ');
		} else 
			putchar(c);
	}
	puts("");
}

/* tabstop: return number of spaces required to reach next tabstop 
     assumes fixed set of tabstops every tabstop columns */
int tabspace(int nchar, int tabstop)
{
	return tabstop - (nchar % tabstop);
}
