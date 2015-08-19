#include <ctype.h>
#include <stdio.h>

/* prototypes defined in getch.c */
int getch(void);
void ungetch(int);

/* prototypes */
int getint(int *);

int main(void)
{
	int val, res;

	res = getint(&val);
	printf("res: %d\tval: %d\n", res, val);
	return 0;
}

/* getint: get next integer from input into *pn */
int getint(int *pn)
{
	int c, sign;

	while (isspace(c = getch())) /* skip whitespace */
		;
	if (!isdigit(c) && c != EOF && c != '+' && c != '-') {
		ungetch(c);	/* it is not a number */
		return 0;
	}
	sign = (c == '-') ? -1 : 1;
	if ( c == '+' || c == '-') {
		c = getch();
		if (!isdigit(c)) {
			ungetch(c); /* put back the '+' or '-' */
			ungetch(c); /* put back the next character */
			return 0;
		}
	}
	for (*pn = 0; isdigit(c); c = getch())
		*pn = 10 * *pn + (c - '0');
	*pn *= sign;
	if (c != EOF)
		ungetch(c);
	return c;
}
