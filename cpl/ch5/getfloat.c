#include <ctype.h>
#include <stdio.h>

/* prototypes defined in getch.c */
int getch(void);
void ungetch(int);

/* prototypes */
int getfloat(float *);

int main(void)
{
	int res;
	float val;

	res = getfloat(&val);
	printf("res: %d\tval: %f\n", res, val);
	return 0;
}

/* getfloat: get next integer from input into *pn
 *   return character after end of float */
int getfloat(float *pn)
{
	int c, sign, power;

	while (isspace(c = getch())) /* skip whitespace */
		;
	if (!isdigit(c) && c != EOF && c != '+' && c != '-' && c != '.') {
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
	power = 1;
	if (c == '.') {
		for ((c = getch()) ; isdigit(c); c = getch()) {
			*pn = 10 * *pn + (c - '0');
			power *= 10;
		}
	}
	*pn = *pn * sign / power;
	if (c != EOF)
		ungetch(c);
	return c;
}
