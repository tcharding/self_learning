#include "calc.h"
#include <ctype.h>
#include <stdarg.h>
#include <string.h>

/* attr: Kernighan and Ritchie, second edition  */

enum {
	FALSE = 0,
	TRUE = 1,
	BUFSIZE = 1024,
	EMPTY = 256,		/* invalid ascii character */
};

/* static prototypes */
static int isop(int c);
static int optype(char s[], int len);

/* getop: get next operator or numeric operand, return type */
int getop(char s[])
{
	int i, c, type;

	i = 0;
	while ( (s[i] = c = getch()) == ' ' || c == '\t')
		;		/* skip white space */
	if (c == '-') {		/* '-' could be operator or negative indicator */
		s[++i] = c = getch();
		if (!isdigit(c)) {
			ungetch(c);
			s[i] = '\0';
			return s[0];
		}
	}
	if (isop(c) || c == '\n') {
	        s[i] = '\0';
		return c;
	} else if (isdigit(c) || c == '.') { /* digit */
		type = NUMBER;
		if (isdigit(c))		/* collect integer part */
			while (isdigit(s[++i] = c = getch()))
				;
		if (c == '.')		/* collect fraction part */
			while (isdigit(s[++i] = c = getch()))
				;
	} else {		/* handles any other input */
		while ( (s[++i] = c = getch()) != ' ' && c != '\t' && c != '\n')
			;
		type = optype(s, i-1);
	}
	s[i] = '\0';
	if (c != EOF)
		ungetch(c);
	return type;
}

/* isop: return true if c is an operator */
static int isop(int c)
{
	switch (c) {
	case '*': case '/': case '+': case '%': /* '-' handled in getop() */
		return TRUE;
	}
	return FALSE;
}

/* math_type: return integer code for math type or -1 if not supported */
static int optype(char s[], int n)
{
	int op;
	
	if (strncmp(s, "sin", n) == 0)
		op = SIN;
	else if (strncmp(s, "cos", n) == 0)
		op = COS;
	else if (strncmp(s, "tan", n) == 0)
		op = TAN;
	else if (strncmp(s, "pow", n) == 0)
		op = POW;
	else if (strncmp(s, "ans", n) == 0)
		op = ANS;
	else
		op = NOOP;	/* error */

	return op;
}
