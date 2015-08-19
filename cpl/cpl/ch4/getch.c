#include "calc.h"

/* attr: Kernighan and Ritchie, second edition  */

enum {
	BUFSIZE = 1024,
};

static char buf[BUFSIZE];	/* buffer for ungetch */
static int bufp = 0;		/* stack pointer */

/* getch: get a (possibly pushed back) char */
int getch(void)
{
	return (bufp > 0) ? buf[--bufp] : getchar();
}

/* ungetch: push character back on input */
void ungetch(int c)
{
	if (bufp >= BUFSIZE)
		printf("ungetch: too many characters\n");
	else
		buf[bufp++] = c;
}
