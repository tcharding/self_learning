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
static int optype(char *s);

/* getop: get next operator or numeric operand, return type */
int getop(char *op)
{
	int c;

	while ((c = getchar()) == ' ' || c == '\t')
		;
	if (c == '\n') {
		*op = '\0';
		return DONE;
	} else
		ungetch(c);
	if (scanf("%s", op) == EOF)
	    return EOF;
	return optype(op);
}
/* /\* getop: get next operator or numeric operand, return type *\/ */
/* int getop(char *op) */
/* { */
/* 	int c, type; */
/* 	char *p = op;		 */

/* 	while ((c = getchar()) == ' ' || c == '\t') */
/* 		;		/\* skip white space *\/ */
/* 	*p++ = c; */
/* 	if (c == '-') {       /\* '-' could be operator or negative indicator *\/ */
/* 		c = getch(); */
/* 		if (!isdigit(c)) { */
/* 			ungetch(c); */
/* 			*p = '\0'; */
/* 			return *op; */
/* 		} */
/* 	} */
/* 	if (is_op(c) || c == '\n') { */
/* 	        *p = '\0'; */
/* 		return *op; */
/* 	} else if (isdigit(c) || c == '.') { /\* digit *\/ */
/* 		type = NUMBER; */
/* 		if (isdigit(c))		/\* collect integer part *\/ */
/* 			while (isdigit(c = getch())) */
/* 				*p++ = c; */
/* 		if (c == '.')		/\* collect fraction part *\/ */
/* 			while (isdigit(c = getchar())) */
/* 				*p++ = c; */
/* 	} else {		/\* handles any other input *\/ */
/* 		while (!isspace(c)) { */
/* 			*p++ = c; */
/* 			c = getchar(); */
/* 		} */
/* 		type = optype(op); */
/* 	} */
/* 	*p = '\0'; */
/* 	if (c != EOF) */
/* 		ungetch(c); */
/* 	return type; */
/* } */



/* math_type: return integer code for math type or -1 if not supported */
static int optype(char *s)
{
	int op;
	char ops[] = {'+', '-', '*', '/', '\0'};
	char *p;

	if (*s == '\n')
		return DONE;
	if (isdigit(*s))	/* handle digits */
		return NUMBER;
	for (p = ops; *p; ++p)	/* handle operators */
		if (*s == *p)
			return *s;
	if (strcmp(s, "sin") == 0) /* handle math functions */
		op = SIN;
	else if (strcmp(s, "cos") == 0)
		op = COS;
	else if (strcmp(s, "tan") == 0)
		op = TAN;
	else if (strcmp(s, "pow") == 0)
		op = POW;
	else if (strcmp(s, "ans") == 0)
		op = ANS;
	else
		op = NOOP;	/* error */

	return op;
}

/* t_optype: test optype */
void t_optype(void)
{
	if (optype("123") != NUMBER)
		fprintf(stderr, "fail: number\n");
	if (optype("/") != '/')
		fprintf(stderr, "fail: op\n");
	if (optype("pow") != POW)
		fprintf(stderr, "fail: pow \n");
	if (optype("\n") != DONE)
		fprintf(stderr, "fail: newline \n");
	
}
