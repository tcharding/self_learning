#include <stdio.h>

enum { TABSTOP = 4 };

void entab(void);
void detab(void);
int tabspace(int nchar, int tabstop);

/* getch.c */
int getch(void);
void ungetch(int c);

/* replace blanks with minimum number of tabs and spaces */
int main(int argc, char *argv[])
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
	return 0;
}
