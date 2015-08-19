#include "calc.h"

/* 
 * Reverse Polish Calculator
 *
 * Based on code in Kernighan and Ritchie, second edition 
 */

enum { MAXVAL = 100 };

static int sp = 0;		/* next free stack position */
double val[MAXVAL];		/* value stack */

/* push: push f onto value stack */
void push(double f)
{

	if (sp < MAXVAL)
		val[sp++] = f;
	else
		fprintf(stderr, "error: stack full, cannot push %f\n", f);
}

/* pop: pop and return top value from stack */
double pop(void)
{

	if (sp > 0)
		return val[--sp];
	else 
		fprintf(stderr, "error: stack empty\n");

	return 0.0;		/* error */
}

/* peek: print top element of stack */
void peek(void)
{
	
	if (sp > 0)
		printf("%.8f\n", val[sp-1]);
	else 
		fprintf(stderr, "error: stack empty\n");
}

/* dup_top: duplicate top entry of stack */
void dup_top(void)
{
	
	if (sp > 0)
		push(val[sp-1]);
	else 
		fprintf(stderr, "error: stack empty\n");
}

/* swap: swap top two elements of stack */
void swap(void)
{
	double first, second;

	first = pop();
	second = pop();
	push(first);
	push(second);
}

/* clear: clear stack */
void clear(void)
{
	sp = 0;
}
