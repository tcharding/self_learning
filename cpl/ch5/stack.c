#include <stdlib.h>
#include <stdio.h>

/* attr: Kernighan and Ritchie, second edition */

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
